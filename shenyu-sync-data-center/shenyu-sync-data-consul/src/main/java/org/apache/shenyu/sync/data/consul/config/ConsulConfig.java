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

package org.apache.shenyu.sync.data.consul.config;

import java.util.Objects;

/**
 * The type Consul config.
 */
public class ConsulConfig {

    private String url;

    private int waitTime;

    private int watchDelay;

    /**
     * get url.
     *
     * @return url
     */
    public String getUrl() {
        return url;
    }

    /**
     * set url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * get waitTime.
     *
     * @return waitTime
     */
    public int getWaitTime() {
        return waitTime;
    }

    /**
     * set waitTime.
     *
     * @param waitTime waitTime
     */
    public void setWaitTime(final int waitTime) {
        this.waitTime = waitTime;
    }

    /**
     * get watchDelay.
     *
     * @return watchDelay
     */
    public int getWatchDelay() {
        return watchDelay;
    }

    /**
     * set watchDelay.
     *
     * @param watchDelay watchDelay
     */
    public void setWatchDelay(final int watchDelay) {
        this.watchDelay = watchDelay;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ConsulConfig that = (ConsulConfig) o;
        return waitTime == that.waitTime
                && watchDelay == that.watchDelay
                && Objects.equals(url, that.url);
    }

    @Override
    public int hashCode() {
        return Objects.hash(url, waitTime, watchDelay);
    }

    @Override
    public String toString() {
        return "ConsulConfig{"
                + "url='"
                + url
                + '\''
                + ", waitTime="
                + waitTime
                + ", watchDelay="
                + watchDelay
                + '}';
    }
}
