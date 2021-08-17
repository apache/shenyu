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

package org.apache.shenyu.common.config;

import java.io.Serializable;
import java.util.Objects;
import java.util.Properties;

/**
 * The monitor configuration for influxdb.
 */
public class MonitorConfig implements Serializable {

    private static final long serialVersionUID = -9186727374865514837L;

    private String metricsName;

    private String host;

    private Integer port;

    private Boolean async;

    private Integer threadCount;

    private Properties props;

    /**
     * get metricsName.
     *
     * @return metricsName
     */
    public String getMetricsName() {
        return metricsName;
    }

    /**
     * set metricsName.
     *
     * @param metricsName metricsName
     */
    public void setMetricsName(final String metricsName) {
        this.metricsName = metricsName;
    }

    /**
     * get host.
     *
     * @return host
     */
    public String getHost() {
        return host;
    }

    /**
     * set host.
     *
     * @param host host
     */
    public void setHost(final String host) {
        this.host = host;
    }

    /**
     * get port.
     *
     * @return port
     */
    public Integer getPort() {
        return port;
    }

    /**
     * set port.
     *
     * @param port port
     */
    public void setPort(final Integer port) {
        this.port = port;
    }

    /**
     * get async.
     *
     * @return async
     */
    public Boolean getAsync() {
        return async;
    }

    /**
     * set async.
     *
     * @param async async
     */
    public void setAsync(final Boolean async) {
        this.async = async;
    }

    /**
     * get threadCount.
     *
     * @return threadCount
     */
    public Integer getThreadCount() {
        return threadCount;
    }

    /**
     * set threadCount.
     *
     * @param threadCount threadCount
     */
    public void setThreadCount(final Integer threadCount) {
        this.threadCount = threadCount;
    }

    /**
     * get props.
     *
     * @return props
     */
    public Properties getProps() {
        return props;
    }

    /**
     * set props.
     *
     * @param props props
     */
    public void setProps(final Properties props) {
        this.props = props;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        MonitorConfig that = (MonitorConfig) o;
        return Objects.equals(metricsName, that.metricsName) && Objects.equals(host, that.host)
                && Objects.equals(port, that.port) && Objects.equals(async, that.async)
                && Objects.equals(threadCount, that.threadCount) && Objects.equals(props, that.props);
    }

    @Override
    public int hashCode() {
        return Objects.hash(metricsName, host, port, async, threadCount, props);
    }

    @Override
    public String toString() {
        return "MonitorConfig{"
                + "metricsName='"
                + metricsName
                + '\''
                + ", host='"
                + host
                + '\''
                + ", port="
                + port
                + ", async="
                + async
                + ", threadCount="
                + threadCount
                + ", props="
                + props
                + '}';
    }
}
