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

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
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
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
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
 * <p>The source list is sent as a structured JSON array. A master that predates the
 * structured envelope rejects the request with 400 or 415; the sender then retries with
 * the legacy string-encoded source and remembers the decision for that master address
 * until this node restarts.
 *
 * <p>Forwarding runs synchronously on the publishing thread, so an unreachable master adds up
 * to the configured connect/read timeout to the calling write API; delivery is best-effort
 * and failures are logged with the master identity and outcome.
 */
public class ClusterDataChangedEventForwarder {

    private static final Logger LOG = LoggerFactory.getLogger(ClusterDataChangedEventForwarder.class);

    private static final String FORWARD_PATH = "/cluster/data-change-event";

    private static final int LEGACY_PAYLOAD_VERSION = 1;

    private static final int STRUCTURED_PAYLOAD_VERSION = 2;

    private final RestTemplate restTemplate;

    private final ClusterProperties clusterProperties;

    private final ClusterSelectMasterService clusterSelectMasterService;

    private final ObjectMapper objectMapper;

    /**
     * Master address the envelope decision below was made for.
     */
    private volatile String negotiatedMasterKey;

    /**
     * Whether the master at {@link #negotiatedMasterKey} accepts the structured envelope.
     */
    private volatile Boolean structuredEnvelope;

    /**
     * Instantiates a new cluster data changed event forwarder.
     *
     * @param restTemplate               the rest template used to reach the master node
     * @param clusterProperties          the cluster properties
     * @param clusterSelectMasterService the cluster select master service
     * @param objectMapper               the mapper used to serialize the structured source
     */
    public ClusterDataChangedEventForwarder(final RestTemplate restTemplate,
                                            final ClusterProperties clusterProperties,
                                            final ClusterSelectMasterService clusterSelectMasterService,
                                            final ObjectMapper objectMapper) {
        this.restTemplate = restTemplate;
        this.clusterProperties = clusterProperties;
        this.clusterSelectMasterService = clusterSelectMasterService;
        // The legacy envelope is rendered by Gson, which omits nulls. Keep the structured
        // envelope's content identical so both representations describe the same records.
        this.objectMapper = objectMapper.copy().setSerializationInclusion(JsonInclude.Include.NON_NULL);
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
        final boolean structured = useStructuredEnvelope(url);
        final ForwardResult result = post(url, accessToken, master, event, structured);
        if (ForwardResult.STRUCTURED_REJECTED == result) {
            LOG.warn("master {}:{} rejected the structured event envelope, retrying with the legacy envelope",
                    master.getMasterHost(), master.getMasterPort());
            rememberEnvelope(url, false);
            return ForwardResult.DELIVERED == post(url, accessToken, master, event, false);
        }
        if (ForwardResult.DELIVERED == result) {
            rememberEnvelope(url, structured);
            return true;
        }
        return false;
    }

    private ForwardResult post(final String url, final String accessToken, final ClusterMasterDTO master,
                               final DataChangedEvent event, final boolean structured) {
        final ClusterDataChangedEventPayload payload = buildPayload(event, structured);
        final HttpHeaders headers = new HttpHeaders();
        headers.set(ClusterEventAuthFilter.HEADER, accessToken);
        try {
            final ResponseEntity<String> response =
                    restTemplate.postForEntity(url, new HttpEntity<>(payload, headers), String.class);
            if (response.getStatusCode().is2xxSuccessful()) {
                LOG.info("forwarded DataChangedEvent to master {}:{}, group={}, type={}, size={}, envelope={}, outcome=delivered",
                        master.getMasterHost(), master.getMasterPort(),
                        event.getGroupKey(), event.getEventType(), sourceSize(event), envelopeName(structured));
                return ForwardResult.DELIVERED;
            }
            LOG.info("forwarded DataChangedEvent to master {}:{}, group={}, type={}, size={}, envelope={}, outcome=rejected",
                    master.getMasterHost(), master.getMasterPort(),
                    event.getGroupKey(), event.getEventType(), sourceSize(event), envelopeName(structured));
            return structured && isFormatRejection(response.getStatusCode().value())
                    ? ForwardResult.STRUCTURED_REJECTED : ForwardResult.FAILED;
        } catch (final HttpClientErrorException ex) {
            LOG.warn("failed to forward DataChangedEvent to master {}:{}, group={}, type={}, size={}, envelope={}, status={}, outcome=failed",
                    master.getMasterHost(), master.getMasterPort(),
                    event.getGroupKey(), event.getEventType(), sourceSize(event), envelopeName(structured),
                    ex.getStatusCode().value(), ex);
            return structured && isFormatRejection(ex.getStatusCode().value())
                    ? ForwardResult.STRUCTURED_REJECTED : ForwardResult.FAILED;
        } catch (final RuntimeException ex) {
            LOG.warn("failed to forward DataChangedEvent to master {}:{}, group={}, type={}, size={}, envelope={}, outcome=failed",
                    master.getMasterHost(), master.getMasterPort(),
                    event.getGroupKey(), event.getEventType(), sourceSize(event), envelopeName(structured), ex);
            return ForwardResult.FAILED;
        }
    }

    private ClusterDataChangedEventPayload buildPayload(final DataChangedEvent event, final boolean structured) {
        final ClusterDataChangedEventPayload payload = new ClusterDataChangedEventPayload();
        payload.setGroupKey(event.getGroupKey().name());
        payload.setEventType(event.getEventType().name());
        payload.setNamespaceId(event.getNamespaceId());
        if (structured) {
            payload.setStructuredSource(objectMapper.valueToTree(event.getSource()));
            payload.setPayloadVersion(STRUCTURED_PAYLOAD_VERSION);
        } else {
            payload.setSource(GsonUtils.getInstance().toJson(event.getSource()));
            payload.setPayloadVersion(LEGACY_PAYLOAD_VERSION);
        }
        return payload;
    }

    private boolean useStructuredEnvelope(final String masterKey) {
        final Boolean negotiated = structuredEnvelope;
        return Objects.isNull(negotiated) || !masterKey.equals(negotiatedMasterKey) || negotiated;
    }

    private void rememberEnvelope(final String masterKey, final boolean structured) {
        this.negotiatedMasterKey = masterKey;
        this.structuredEnvelope = structured;
    }

    private static boolean isFormatRejection(final int status) {
        return HttpStatus.BAD_REQUEST.value() == status || HttpStatus.UNSUPPORTED_MEDIA_TYPE.value() == status;
    }

    private static String envelopeName(final boolean structured) {
        return structured ? "structured" : "legacy";
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

    private enum ForwardResult {
        DELIVERED,
        STRUCTURED_REJECTED,
        FAILED
    }
}
