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

package org.apache.shenyu.sync.data.http.config;

import java.util.Objects;

/**
 * The type Http config.
 */
public class HttpConfig {

    private String url;

    private Integer delayTime;

    private Integer connectionTimeout;

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
     * get delayTime.
     *
     * @return delayTime
     */
    public Integer getDelayTime() {
        return delayTime;
    }

    /**
     * set delayTime.
     *
     * @param delayTime delayTime
     */
    public void setDelayTime(final Integer delayTime) {
        this.delayTime = delayTime;
    }

    /**
     * get connectionTimeout.
     *
     * @return connectionTimeout
     */
    public Integer getConnectionTimeout() {
        return connectionTimeout;
    }

    /**
     * set connectionTimeout.
     *
     * @param connectionTimeout connectionTimeout
     */
    public void setConnectionTimeout(final Integer connectionTimeout) {
        this.connectionTimeout = connectionTimeout;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        HttpConfig that = (HttpConfig) o;
        return Objects.equals(url, that.url)
                && Objects.equals(delayTime, that.delayTime)
                && Objects.equals(connectionTimeout, that.connectionTimeout);
    }

    @Override
    public int hashCode() {
        return Objects.hash(url, delayTime, connectionTimeout);
    }

    @Override
    public String toString() {
        return "HttpConfig{"
                + "url='"
                + url
                + '\''
                + ", delayTime="
                + delayTime
                + ", connectionTimeout="
                + connectionTimeout
                + '}';
    }
}
