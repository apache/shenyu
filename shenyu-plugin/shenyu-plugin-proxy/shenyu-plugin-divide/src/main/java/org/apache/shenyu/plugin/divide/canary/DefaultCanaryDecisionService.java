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

package org.apache.shenyu.plugin.divide.canary;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfig;
import org.apache.shenyu.common.dto.convert.rule.canary.StickyKeyConfig;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.plugin.base.condition.data.ParameterDataFactory;
import org.apache.shenyu.plugin.base.condition.strategy.MatchStrategyFactory;
import org.springframework.web.server.ServerWebExchange;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;

/**
 * Deterministic request partitioning using conditions and SHA-256 buckets.
 */
public class DefaultCanaryDecisionService implements CanaryDecisionService {

    @Override
    public CanaryDecision decide(final ServerWebExchange exchange, final String ruleId, final CanaryConfig config) {
        Objects.requireNonNull(config, "Canary configuration is required");
        if (!config.isEnabled()) {
            return CanaryDecision.STABLE;
        }
        int percentage = config.getPercentage();
        if (percentage < 0 || percentage > 100) {
            throw new IllegalArgumentException("Canary percentage must be between 0 and 100");
        }
        if (percentage == 0) {
            return CanaryDecision.STABLE;
        }
        int matchMode = Objects.isNull(config.getMatchMode()) ? MatchModeEnum.AND.getCode() : config.getMatchMode();
        if (CollectionUtils.isNotEmpty(config.getConditions())
                && !MatchStrategyFactory.match(matchMode, config.getConditions(), exchange)) {
            return CanaryDecision.STABLE;
        }
        if (percentage == 100) {
            return CanaryDecision.CANARY;
        }
        String key = readStickyKey(exchange, config.getStickyKey());
        if (StringUtils.isBlank(key)) {
            return CanaryDecision.STABLE;
        }
        return hashToBucket(ruleId, key) < percentage * 100 ? CanaryDecision.CANARY : CanaryDecision.STABLE;
    }

    private String readStickyKey(final ServerWebExchange exchange, final StickyKeyConfig stickyKey) {
        if (Objects.isNull(stickyKey) || StringUtils.isBlank(stickyKey.getParamType())) {
            throw new IllegalArgumentException("Canary sticky key parameter type is required");
        }
        // Pass the parameter name through unchanged; the selected source owns its extraction semantics.
        return ParameterDataFactory.builderData(stickyKey.getParamType(), stickyKey.getParamName(), exchange);
    }

    /**
     * Encode each UTF-8 field with a four-byte big-endian byte-length prefix.
     * Interpret the first four SHA-256 bytes as an unsigned big-endian integer.
     * The percentage and upstream topology deliberately do not enter the hash.
     *
     * @param ruleId stable rule identifier
     * @param key request identifier
     * @return bucket from 0 to 9999
     */
    int hashToBucket(final String ruleId, final String key) {
        Objects.requireNonNull(ruleId, "Canary rule identifier is required");
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            updateField(digest, ruleId);
            updateField(digest, key);
            return (int) (Integer.toUnsignedLong(ByteBuffer.wrap(digest.digest()).getInt()) % 10000);
        } catch (NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is unavailable", ex);
        }
    }

    private void updateField(final MessageDigest digest, final String value) {
        byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        digest.update(ByteBuffer.allocate(Integer.BYTES).putInt(bytes.length).array());
        digest.update(bytes);
    }
}
