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

package org.apache.shenyu.plugin.ai.proxy.enhanced.evaluation;

/**
 * Gateway-wide bounds of the AI evaluation sample export (issue #8256). The
 * feature is off unless {@code enabled} is true AND the route opted in; every
 * buffer and registry here is bounded so an operator can never trade unbounded
 * gateway memory for evaluation data.
 */
public class AiEvaluationProperties {

    private boolean enabled;

    private int queueCapacity = 1024;

    private int batchSize = 32;

    private long joinRetentionMillis = 30 * 60 * 1000L;

    private int maxJoinsPerSample = 4;

    private int registryCapacity = 4096;

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public int getQueueCapacity() {
        return queueCapacity;
    }

    public void setQueueCapacity(final int queueCapacity) {
        this.queueCapacity = queueCapacity;
    }

    public int getBatchSize() {
        return batchSize;
    }

    public void setBatchSize(final int batchSize) {
        this.batchSize = batchSize;
    }

    public long getJoinRetentionMillis() {
        return joinRetentionMillis;
    }

    public void setJoinRetentionMillis(final long joinRetentionMillis) {
        this.joinRetentionMillis = joinRetentionMillis;
    }

    public int getMaxJoinsPerSample() {
        return maxJoinsPerSample;
    }

    public void setMaxJoinsPerSample(final int maxJoinsPerSample) {
        this.maxJoinsPerSample = maxJoinsPerSample;
    }

    public int getRegistryCapacity() {
        return registryCapacity;
    }

    public void setRegistryCapacity(final int registryCapacity) {
        this.registryCapacity = registryCapacity;
    }
}
