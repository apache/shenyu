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

package org.apache.shenyu.plugin.ai.proxy.enhanced.admission;

/**
 * Opt-in per-gateway concurrency admission for AI proxy traffic (issue #8254).
 *
 * <p>Two independent pool dimensions gate an invocation: the trusted-tenant pool
 * (one logical permit per invocation, held until the terminal signal) and the
 * model pool (one target permit per actual attempt, released when the attempt
 * ends — for streaming that is body completion, not response headers). Every
 * buffer is finite; a misconfigured bound fails fast instead of silently
 * disabling protection. These caps are request admission, distinct from the
 * Netty transport connection pool.</p>
 */
public class AiConcurrencyAdmissionProperties {

    private boolean enabled;

    private int tenantMaxInflight = 64;

    private int tenantMaxWaiting = 16;

    private int modelMaxInflight = 64;

    private int modelMaxWaiting = 16;

    private long maxWaitMillis = 5000L;

    private int maxPools = 256;

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public int getTenantMaxInflight() {
        return tenantMaxInflight;
    }

    public void setTenantMaxInflight(final int tenantMaxInflight) {
        this.tenantMaxInflight = tenantMaxInflight;
    }

    public int getTenantMaxWaiting() {
        return tenantMaxWaiting;
    }

    public void setTenantMaxWaiting(final int tenantMaxWaiting) {
        this.tenantMaxWaiting = tenantMaxWaiting;
    }

    public int getModelMaxInflight() {
        return modelMaxInflight;
    }

    public void setModelMaxInflight(final int modelMaxInflight) {
        this.modelMaxInflight = modelMaxInflight;
    }

    public int getModelMaxWaiting() {
        return modelMaxWaiting;
    }

    public void setModelMaxWaiting(final int modelMaxWaiting) {
        this.modelMaxWaiting = modelMaxWaiting;
    }

    public long getMaxWaitMillis() {
        return maxWaitMillis;
    }

    public void setMaxWaitMillis(final long maxWaitMillis) {
        this.maxWaitMillis = maxWaitMillis;
    }

    public int getMaxPools() {
        return maxPools;
    }

    public void setMaxPools(final int maxPools) {
        this.maxPools = maxPools;
    }

    /**
     * Fail fast on an unusable bound: a zero or negative in-flight cap would
     * either admit nothing or admit everything, and a negative bound would
     * corrupt the waiting accounting.
     */
    public void validate() {
        if (tenantMaxInflight <= 0 || modelMaxInflight <= 0) {
            throw new IllegalStateException("admission in-flight caps must be positive");
        }
        if (tenantMaxWaiting < 0 || modelMaxWaiting < 0) {
            throw new IllegalStateException("admission waiting bounds must not be negative");
        }
        if (maxWaitMillis < 0) {
            throw new IllegalStateException("admission maxWaitMillis must not be negative");
        }
        if (maxPools <= 0) {
            throw new IllegalStateException("admission maxPools must be positive");
        }
    }
}
