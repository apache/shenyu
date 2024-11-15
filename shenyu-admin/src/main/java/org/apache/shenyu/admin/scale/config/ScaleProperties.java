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

package org.apache.shenyu.admin.scale.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "shenyu.k8s.scale")
public class ScaleProperties {

    private boolean enabled = true;

    private long monitorInterval;

    private int poolSize;

    /**
     * isEnabled.
     *
     * @return boolean
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * setEnabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * getMonitorInterval.
     *
     * @return long
     */
    public long getMonitorInterval() {
        return monitorInterval;
    }

    /**
     * setMonitorInterval.
     *
     * @param monitorInterval monitorInterval
     */
    public void setMonitorInterval(final long monitorInterval) {
        this.monitorInterval = monitorInterval;
    }

    /**
     * getPoolSize.
     *
     * @return int
     */
    public int getPoolSize() {
        return poolSize;
    }

    /**
     * setPoolSize.
     *
     * @param poolSize poolSize
     */
    public void setPoolSize(final int poolSize) {
        this.poolSize = poolSize;
    }
}
