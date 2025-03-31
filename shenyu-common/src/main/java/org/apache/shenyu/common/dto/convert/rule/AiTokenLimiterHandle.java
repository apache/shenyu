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

package org.apache.shenyu.common.dto.convert.rule;

import org.apache.shenyu.common.enums.AiTokenLimiterEnum;
import org.apache.shenyu.common.enums.TimeWindowEnum;

import java.util.Objects;

/**
 * AiTokenLimiterHandle.
 */
public class AiTokenLimiterHandle {
    /**
     * key resolver enum.
     */
    private String aiTokenLimitType;
    
    /**
     * time window seconds.
     */
    private Long timeWindowSeconds;
    
    /**
     * key name.
     */
    private String keyName;
    
    /**
     * token limit.
     */
    private Long tokenLimit;
    
    /**
     * apiTokenLimitKey.
     *
     * @return apiTokenLimitKey apiTokenLimitKey
     */
    public String getAiTokenLimitType() {
        return aiTokenLimitType;
    }
    
    /**
     * set apiTokenLimitKey.
     *
     * @param aiTokenLimitType apiTokenLimitKey
     */
    public void setAiTokenLimitType(final String aiTokenLimitType) {
        this.aiTokenLimitType = aiTokenLimitType;
    }
    
    /**
     * get timeWindowName.
     *
     * @return timeWindowName time window name
     */
    public Long getTimeWindowSeconds() {
        return timeWindowSeconds;
    }
    
    /**
     * set timeWindowName.
     *
     * @param timeWindowSeconds timeWindowName
     */
    public void setTimeWindowSeconds(final Long timeWindowSeconds) {
        this.timeWindowSeconds = timeWindowSeconds;
    }
    
    /**
     * get keyName.
     *
     * @return keyName key name
     */
    public String getKeyName() {
        return keyName;
    }
    
    /**
     * set keyName.
     *
     * @param keyName keyName
     */
    public void setKeyName(final String keyName) {
        this.keyName = keyName;
    }
    
    /**
     * get tokenLimit.
     *
     * @return tokenLimit token limit
     */
    public Long getTokenLimit() {
        return tokenLimit;
    }
    
    /**
     * set tokenLimit.
     *
     * @param tokenLimit tokenLimit
     */
    public void setTokenLimit(final Long tokenLimit) {
        this.tokenLimit = tokenLimit;
    }
    
    /**
     * new default instance.
     *
     * @return AiTokenLimiterHandle
     */
    public static AiTokenLimiterHandle newDefaultInstance() {
        AiTokenLimiterHandle aiTokenLimiterHandle = new AiTokenLimiterHandle();
        aiTokenLimiterHandle.setAiTokenLimitType(AiTokenLimiterEnum.CONTEXT_PATH.getName());
        aiTokenLimiterHandle.setTimeWindowSeconds(TimeWindowEnum.MINUTE.getSeconds());
        aiTokenLimiterHandle.setKeyName("default");
        aiTokenLimiterHandle.setTokenLimit(100L);
        return aiTokenLimiterHandle;
    }
    
    @Override
    public String toString() {
        return "AiTokenLimiterHandle{"
                + "keyResolverName='" + aiTokenLimitType + '\''
                + ", timeWindowSeconds=" + timeWindowSeconds
                + ", keyName='" + keyName + '\''
                + ", tokenLimit=" + tokenLimit
                + '}';
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof final AiTokenLimiterHandle that)) {
            return false;
        }
        return aiTokenLimitType.equals(that.aiTokenLimitType)
                && timeWindowSeconds.equals(that.timeWindowSeconds)
                && keyName.equals(that.keyName)
                && tokenLimit.equals(that.tokenLimit);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(aiTokenLimitType, timeWindowSeconds, keyName, tokenLimit);
    }
    
}
