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

package org.apache.shenyu.admin.service.impl;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ObservabilityProperties;
import org.apache.shenyu.admin.model.vo.InstanceDiagnosticsVO;
import org.apache.shenyu.admin.model.vo.InstanceDiagnosticsVO.ObservabilityLink;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Builds instance diagnostics: correlation metadata plus navigation links to the
 * operator-configured metrics, logging and tracing backends.
 *
 * <p>High-volume telemetry stays in the configured backend; this service only
 * resolves URL templates with URL-encoded instance correlation values and an
 * explicit time window, so no credentials or sensitive payload data are embedded
 * by Admin and no telemetry data is stored in the Admin database. Missing backend
 * configuration, missing correlation data on legacy instances and invalid
 * templates are reported as distinct link states instead of broken links.</p>
 */
@Component
public class InstanceDiagnosticsService {

    private static final Logger LOG = LoggerFactory.getLogger(InstanceDiagnosticsService.class);

    private static final long DEFAULT_WINDOW_MILLIS = 15 * 60 * 1000L;

    private static final String PLACEHOLDER_NAMESPACE = "{namespace}";

    private static final String PLACEHOLDER_INSTANCE_IP = "{instanceIp}";

    private static final String PLACEHOLDER_INSTANCE_PORT = "{instancePort}";

    private static final String PLACEHOLDER_INSTANCE_TYPE = "{instanceType}";

    private static final String PLACEHOLDER_BOOT_TIME = "{bootTime}";

    private static final String PLACEHOLDER_START_TIME = "{startTime}";

    private static final String PLACEHOLDER_END_TIME = "{endTime}";

    private static final String PLACEHOLDER_START_TIME_ISO = "{startTimeIso}";

    private static final String PLACEHOLDER_END_TIME_ISO = "{endTimeIso}";

    private final InstanceInfoService instanceInfoService;

    private final InstanceCheckService instanceCheckService;

    private final ObservabilityProperties observabilityProperties;

    public InstanceDiagnosticsService(final InstanceInfoService instanceInfoService,
                                      final InstanceCheckService instanceCheckService,
                                      final ObservabilityProperties observabilityProperties) {
        this.instanceInfoService = instanceInfoService;
        this.instanceCheckService = instanceCheckService;
        this.observabilityProperties = observabilityProperties;
    }

    /**
     * Build diagnostics for one instance.
     *
     * @param id        instance id
     * @param startTime optional time window start, epoch millis, defaults to the boot time
     *                  when known or the last {@value #DEFAULT_WINDOW_MILLIS} millis
     * @param endTime   optional time window end, epoch millis, defaults to now
     * @return the diagnostics view, or null when the instance does not exist
     */
    public InstanceDiagnosticsVO getInstanceDiagnostics(final String id, final Long startTime, final Long endTime) {
        InstanceInfoVO instanceInfoVO = instanceInfoService.findById(id);
        if (Objects.isNull(instanceInfoVO)) {
            return null;
        }
        InstanceInfoVO beatInfo = instanceCheckService.getInstanceHealthBeatInfo(instanceCheckService.getInstanceKey(instanceInfoVO));
        if (Objects.nonNull(beatInfo)) {
            instanceInfoVO.setInstanceState(beatInfo.getInstanceState());
            instanceInfoVO.setLastHeartBeatTime(beatInfo.getLastHeartBeatTime());
        }
        Long bootTime = parseBootTime(instanceInfoVO.getInstanceInfo());
        long resolvedEnd = Objects.nonNull(endTime) && endTime > 0 ? endTime : System.currentTimeMillis();
        long resolvedStart;
        if (Objects.nonNull(startTime) && startTime > 0) {
            resolvedStart = startTime;
        } else if (Objects.nonNull(bootTime) && bootTime > resolvedEnd - DEFAULT_WINDOW_MILLIS) {
            resolvedStart = bootTime;
        } else {
            resolvedStart = resolvedEnd - DEFAULT_WINDOW_MILLIS;
        }
        if (resolvedStart > resolvedEnd) {
            resolvedStart = resolvedEnd;
        }
        InstanceDiagnosticsVO diagnosticsVO = new InstanceDiagnosticsVO();
        diagnosticsVO.setInstanceId(id);
        diagnosticsVO.setInstanceKey(instanceCheckService.getInstanceKey(instanceInfoVO));
        diagnosticsVO.setNamespaceId(instanceInfoVO.getNamespaceId());
        diagnosticsVO.setInstanceIp(instanceInfoVO.getInstanceIp());
        diagnosticsVO.setInstancePort(instanceInfoVO.getInstancePort());
        diagnosticsVO.setInstanceType(instanceInfoVO.getInstanceType());
        diagnosticsVO.setInstanceState(instanceInfoVO.getInstanceState());
        diagnosticsVO.setLastHeartBeatTime(instanceInfoVO.getLastHeartBeatTime());
        diagnosticsVO.setBootTime(bootTime);
        diagnosticsVO.setStartTime(resolvedStart);
        diagnosticsVO.setEndTime(resolvedEnd);
        List<ObservabilityLink> links = new ArrayList<>(3);
        links.add(resolveLink(ObservabilityLink.TYPE_METRICS, observabilityProperties.getMetricsUrlTemplate(),
                instanceInfoVO, bootTime, resolvedStart, resolvedEnd));
        links.add(resolveLink(ObservabilityLink.TYPE_LOGS, observabilityProperties.getLogsUrlTemplate(),
                instanceInfoVO, bootTime, resolvedStart, resolvedEnd));
        links.add(resolveLink(ObservabilityLink.TYPE_TRACE, observabilityProperties.getTraceUrlTemplate(),
                instanceInfoVO, bootTime, resolvedStart, resolvedEnd));
        diagnosticsVO.setLinks(links);
        return diagnosticsVO;
    }

    private ObservabilityLink resolveLink(final String type, final String template, final InstanceInfoVO instanceInfoVO,
                                          final Long bootTime, final long startTime, final long endTime) {
        if (StringUtils.isBlank(template)) {
            return new ObservabilityLink(type, ObservabilityLink.STATE_NOT_CONFIGURED, null);
        }
        if (template.contains(PLACEHOLDER_BOOT_TIME) && Objects.isNull(bootTime)) {
            // legacy instances do not report the boot identity, do not invent one
            return new ObservabilityLink(type, ObservabilityLink.STATE_MISSING_INSTANCE_DATA, null);
        }
        Map<String, String> values = new LinkedHashMap<>();
        values.put(PLACEHOLDER_NAMESPACE, instanceInfoVO.getNamespaceId());
        values.put(PLACEHOLDER_INSTANCE_IP, instanceInfoVO.getInstanceIp());
        values.put(PLACEHOLDER_INSTANCE_PORT, instanceInfoVO.getInstancePort());
        values.put(PLACEHOLDER_INSTANCE_TYPE, instanceInfoVO.getInstanceType());
        values.put(PLACEHOLDER_BOOT_TIME, Objects.isNull(bootTime) ? null : String.valueOf(bootTime));
        values.put(PLACEHOLDER_START_TIME, String.valueOf(startTime));
        values.put(PLACEHOLDER_END_TIME, String.valueOf(endTime));
        values.put(PLACEHOLDER_START_TIME_ISO, formatIso(startTime));
        values.put(PLACEHOLDER_END_TIME_ISO, formatIso(endTime));
        String url = template;
        for (Map.Entry<String, String> entry : values.entrySet()) {
            url = url.replace(entry.getKey(), encode(entry.getValue()));
        }
        // substituted values are already encoded, only template literals may contain raw spaces
        url = url.replace(" ", "%20");
        if (!isSafeNavigationUrl(url)) {
            LOG.warn("observability {} url template is rejected, it is not a plain http/https url or embeds credentials", type);
            return new ObservabilityLink(type, ObservabilityLink.STATE_INVALID_TEMPLATE, null);
        }
        return new ObservabilityLink(type, ObservabilityLink.STATE_READY, url);
    }

    private static String formatIso(final long epochMillis) {
        return DateTimeFormatter.ISO_INSTANT.format(Instant.ofEpochMilli(epochMillis));
    }

    private static String encode(final String value) {
        return StringUtils.isEmpty(value) ? StringUtils.EMPTY : URLEncoder.encode(value, StandardCharsets.UTF_8);
    }

    private static boolean isSafeNavigationUrl(final String url) {
        try {
            URI uri = new URI(url);
            String scheme = uri.getScheme();
            return (Objects.equals("http", scheme) || Objects.equals("https", scheme)) && Objects.isNull(uri.getUserInfo());
        } catch (URISyntaxException | IllegalArgumentException ex) {
            return false;
        }
    }

    private Long parseBootTime(final String instanceInfo) {
        if (StringUtils.isBlank(instanceInfo)) {
            return null;
        }
        try {
            Map<String, Object> infoMap = GsonUtils.getInstance().convertToMap(instanceInfo);
            if (Objects.isNull(infoMap)) {
                return null;
            }
            Object bootTime = infoMap.get(Constants.BOOT_TIME);
            return bootTime instanceof Number ? ((Number) bootTime).longValue() : null;
        } catch (RuntimeException ex) {
            // unparsable instance info must not break the diagnostics view
            LOG.debug("failed to parse boot time from instance info: {}", instanceInfo, ex);
            return null;
        }
    }
}
