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

package org.apache.shenyu.loadbalancer.spi;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.GrayCondition;
import org.apache.shenyu.common.dto.convert.rule.GrayConfig;
import org.apache.shenyu.common.dto.convert.rule.MetadataMatch;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Gray load balancer impl.
 */
@Join
public class GrayLoadBalancer implements LoadBalancer {

    /**
     * The constant GRAY_CONFIG_KEY.
     */
    public static final String GRAY_CONFIG_KEY = "grayConfig";

    /**
     * The constant GRAY.
     */
    public static final String GRAY = "gray";

    private static final Logger LOG = LoggerFactory.getLogger(GrayLoadBalancer.class);

    private static final int MAX_PATTERN_CACHE_SIZE = 1024;

    private static final Map<String, Pattern> PATTERN_CACHE = new ConcurrentHashMap<>();

    @Override
    public Upstream select(final List<Upstream> upstreamList, final LoadBalanceData data) {
        if (CollectionUtils.isEmpty(upstreamList)) {
            return null;
        }

        GrayConfig grayConfig = getGrayConfig(data);
        if (Objects.isNull(grayConfig)) {
            return delegateSelect(upstreamList, "random", data);
        }

        MetadataMatch metadataMatch = grayConfig.getMetadataMatch();
        List<Upstream> grayCandidates = upstreamList.stream()
                .filter(Upstream::isGray)
                .collect(Collectors.toList());
        List<Upstream> grayList = grayCandidates.stream()
                .filter(upstream -> matchMetadata(upstream, metadataMatch))
                .collect(Collectors.toList());
        List<Upstream> normalList = upstreamList.stream()
                .filter(upstream -> !upstream.isGray())
                .collect(Collectors.toList());

        boolean routeToGray = shouldRouteToGray(grayConfig, data);

        List<Upstream> targetList;
        if (routeToGray && !grayList.isEmpty()) {
            targetList = grayList;
        } else {
            if (routeToGray && grayList.isEmpty()) {
                LOG.warn("Gray routing matched but no gray upstreams available, falling back to normal");
            }
            targetList = normalList;
        }
        if (targetList.isEmpty()) {
            targetList = upstreamList;
        }

        Upstream selected = delegateSelect(targetList, grayConfig.getLoadBalance(), data);
        if (LOG.isDebugEnabled() && Objects.nonNull(selected)) {
            LOG.debug("Gray select: routeToGray={}, target={}, grayPool={}, normalPool={}",
                    routeToGray, selected.getUrl(), grayList.size(), normalList.size());
        }
        return selected;
    }

    private boolean matchMetadata(final Upstream upstream, final MetadataMatch metadataMatch) {
        if (Objects.isNull(metadataMatch) || StringUtils.isBlank(metadataMatch.getKey()) || Objects.isNull(metadataMatch.getValue())) {
            return true;
        }
        Map<String, String> metadata = upstream.getMetadata();
        return Objects.nonNull(metadata) && metadataMatch.getValue().equals(metadata.get(metadataMatch.getKey()));
    }

    private GrayConfig getGrayConfig(final LoadBalanceData data) {
        if (Objects.isNull(data) || Objects.isNull(data.getAttributes())) {
            return null;
        }
        Object config = data.getAttributes().get(GRAY_CONFIG_KEY);
        if (config instanceof GrayConfig) {
            return (GrayConfig) config;
        }
        return null;
    }

    private boolean shouldRouteToGray(final GrayConfig config, final LoadBalanceData data) {
        List<GrayCondition> conditions = config.getConditions();
        if (Objects.isNull(conditions) || conditions.isEmpty()) {
            return false;
        }
        boolean condMatched = false;
        for (GrayCondition condition : conditions) {
            if (matchCondition(condition, data)) {
                condMatched = true;
                break;
            }
        }
        if (!condMatched) {
            return false;
        }
        int percent = Math.max(0, Math.min(100, config.getPercent()));
        if (percent > 0) {
            return ThreadLocalRandom.current().nextInt(100) < percent;
        }
        return false;
    }

    private boolean matchCondition(final GrayCondition condition, final LoadBalanceData data) {
        try {
            return doMatchCondition(condition, data);
        } catch (final Exception e) {
            LOG.warn("Gray condition match failed, treating as not matched: {}", e.getMessage());
            return false;
        }
    }

    private boolean doMatchCondition(final GrayCondition condition, final LoadBalanceData data) {
        String actual = extractParam(condition.getParamType(), condition.getParamName(), data);
        String operator = condition.getOperator();
        if (Objects.isNull(operator)) {
            return false;
        }
        if ("isBlank".equals(operator)) {
            return Objects.isNull(actual) || StringUtils.isBlank(actual);
        }
        if (Objects.isNull(actual)) {
            return false;
        }
        String expected = condition.getParamValue();
        if (Objects.isNull(expected)) {
            return false;
        }
        switch (operator) {
            case "=":
                return actual.equals(expected);
            case "match":
                return actual.contains(expected);
            case "regex":
                if (PATTERN_CACHE.size() >= MAX_PATTERN_CACHE_SIZE) {
                    PATTERN_CACHE.clear();
                }
                return PATTERN_CACHE.computeIfAbsent(expected, Pattern::compile)
                        .matcher(actual).matches();
            case "contains":
                return actual.contains(expected);
            case "exclude":
                return !actual.contains(expected);
            case "startsWith":
                return actual.startsWith(expected);
            case "endsWith":
                return actual.endsWith(expected);
            default:
                return false;
        }
    }

    private String extractParam(final String type, final String name, final LoadBalanceData data) {
        if (Objects.isNull(type)) {
            return null;
        }
        switch (type) {
            case "header":
                return getFirstValue(data.getHeaders(), name);
            case "cookie":
                return Objects.nonNull(data.getCookies()) ? data.getCookies().get(name) : null;
            case "query":
                return getFirstValue(data.getQueryParams(), name);
            case "ip":
                return data.getIp();
            default:
                return null;
        }
    }

    private String getFirstValue(final Map<String, Collection<String>> map, final String key) {
        if (Objects.isNull(map) || Objects.isNull(key)) {
            return null;
        }
        // Try exact match first, then case-insensitive fallback.
        // HTTP headers are case-insensitive per RFC 7230, but the map
        // may be a plain HashMap (converted from Spring HttpHeaders).
        Collection<String> values = map.get(key);
        if (Objects.isNull(values) || values.isEmpty()) {
            for (Map.Entry<String, Collection<String>> entry : map.entrySet()) {
                if (key.equalsIgnoreCase(entry.getKey())) {
                    values = entry.getValue();
                    break;
                }
            }
        }
        if (Objects.nonNull(values) && !values.isEmpty()) {
            return values.iterator().next();
        }
        return null;
    }

    private Upstream delegateSelect(final List<Upstream> list, final String algorithm, final LoadBalanceData data) {
        String delegateAlgorithm = algorithm;
        if (StringUtils.isBlank(delegateAlgorithm) || GRAY.equals(delegateAlgorithm)) {
            delegateAlgorithm = "random";
        }
        LoadBalancer delegate = ExtensionLoader.getExtensionLoader(LoadBalancer.class)
                .getJoin(delegateAlgorithm);
        return delegate.select(list, data);
    }
}
