/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.listener;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterDataChangedEventPayload;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.admin.shiro.bean.ClusterEventAuthFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.util.Objects;

/**
 * Forwards locally committed {@link DataChangedEvent}s to the current master admin node.
 *
 * <p>{@code DataChangedEvent} is a local Spring application event. In cluster mode a
 * configuration write accepted by a non-master node would otherwise never reach the
 * listeners running on the master node (websocket push, long polling cache, registry
 * writers), so the change is handed over to the master over HTTP and re-published there.
 *
 * <p>The forward authenticates with a dedicated node credential, independent of request
 * context. Configure HTTPS to protect the credential in transit.
 *
 * <p>Forwarding runs synchronously on the publishing thread, so an unreachable master adds up
 * to the configured connect/read timeout to the calling write API; delivery is best-effort
 * and failures are logged with the master identity and outcome.
 */
public class ClusterDataChangedEventForwarder {

    private static final Logger LOG = LoggerFactory.getLogger(ClusterDataChangedEventForwarder.class);

    private static final String FORWARD_PATH = "/cluster/data-change-event";

    private final RestTemplate restTemplate;

    private final ClusterProperties clusterProperties;

    private final ClusterSelectMasterService clusterSelectMasterService;

    /**
     * Instantiates a new cluster data changed event forwarder.
     *
     * @param restTemplate               the rest template used to reach the master node
     * @param clusterProperties          the cluster properties
     * @param clusterSelectMasterService the cluster select master service
     */
    public ClusterDataChangedEventForwarder(final RestTemplate restTemplate,
                                            final ClusterProperties clusterProperties,
                                            final ClusterSelectMasterService clusterSelectMasterService) {
        this.restTemplate = restTemplate;
        this.clusterProperties = clusterProperties;
        this.clusterSelectMasterService = clusterSelectMasterService;
    }

    /**
     * Forward a committed data change event to the current master node, authenticated with
     * the dedicated cluster credential.
     *
     * @param event the locally committed data change event
     * @return true if the master accepted the event
     */
    public boolean forward(final DataChangedEvent event) {
        final ClusterMasterDTO master = clusterSelectMasterService.getMaster();
        if (Objects.isNull(master) || StringUtils.isBlank(master.getMasterHost()) || StringUtils.isBlank(master.getMasterPort())) {
            LOG.warn("no master available, cannot forward DataChangedEvent, group={}, type={}, size={}",
                    event.getGroupKey(), event.getEventType(), sourceSize(event));
            return false;
        }
        final String accessToken = clusterProperties.getEventSecret();
        if (StringUtils.isBlank(accessToken)) {
            LOG.warn("Cluster event credential is not configured; refusing to forward configuration");
            return false;
        }
        final String url = buildMasterUrl(master);
        final ClusterDataChangedEventPayload payload = new ClusterDataChangedEventPayload(
                event.getGroupKey().name(), event.getEventType().name(),
                GsonUtils.getInstance().toJson(event.getSource()));
        final HttpHeaders headers = new HttpHeaders();
        headers.set(ClusterEventAuthFilter.HEADER, accessToken);
        try {
            final ResponseEntity<String> response =
                    restTemplate.postForEntity(url, new HttpEntity<>(payload, headers), String.class);
            final boolean accepted = response.getStatusCode().is2xxSuccessful();
            LOG.info("forwarded DataChangedEvent to master {}:{}, group={}, type={}, size={}, outcome={}",
                    master.getMasterHost(), master.getMasterPort(),
                    event.getGroupKey(), event.getEventType(), sourceSize(event),
                    accepted ? "delivered" : "rejected");
            return accepted;
        } catch (final RuntimeException ex) {
            LOG.warn("failed to forward DataChangedEvent to master {}:{}, group={}, type={}, size={}, outcome=failed",
                    master.getMasterHost(), master.getMasterPort(),
                    event.getGroupKey(), event.getEventType(), sourceSize(event), ex);
            return false;
        }
    }

    private int sourceSize(final DataChangedEvent event) {
        return Objects.isNull(event.getSource()) ? 0 : event.getSource().size();
    }

    private String buildMasterUrl(final ClusterMasterDTO master) {
        String contextPath = StringUtils.defaultString(master.getContextPath());
        if (StringUtils.isNotEmpty(contextPath) && !contextPath.startsWith("/")) {
            contextPath = "/" + contextPath;
        }
        return clusterProperties.getSchema() + "://" + master.getMasterHost() + ":" + master.getMasterPort()
                + contextPath + FORWARD_PATH;
    }
}
