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

package org.apache.shenyu.springboot.starter.k8s;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.time.Duration;

/**
 * Configuration properties for Kubernetes ingress and endpoints controllers.
 */
@ConfigurationProperties(prefix = "shenyu.k8s.controller")
public class ShenyuKubernetesControllerProperties {

    /**
     * Number of workers used by each controller. Values greater than one rely on the
     * ingress, selector, secret and service relation caches remaining thread-safe.
     */
    private int workerCount = 2;

    /**
     * Period between full informer resynchronizations.
     */
    private Duration resyncPeriod = Duration.ofMinutes(1);

    /**
     * Get worker count.
     *
     * @return worker count
     */
    public int getWorkerCount() {
        return workerCount;
    }

    /**
     * Set worker count.
     *
     * @param workerCount worker count
     */
    public void setWorkerCount(final int workerCount) {
        this.workerCount = workerCount;
    }

    /**
     * Get resync period.
     *
     * @return resync period
     */
    public Duration getResyncPeriod() {
        return resyncPeriod;
    }

    /**
     * Set resync period.
     *
     * @param resyncPeriod resync period
     */
    public void setResyncPeriod(final Duration resyncPeriod) {
        this.resyncPeriod = resyncPeriod;
    }
}
