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

package org.apache.shenyu.metrics.config;

import java.io.Serializable;
import java.util.Objects;
import java.util.Properties;
import java.util.StringJoiner;

/**
 * Metrics config.
 */
public final class MetricsConfig implements Serializable {

    private static final long serialVersionUID = -9222476229902864771L;

    private String metricsName;

    private String host;

    private Integer port;

    private Boolean async;

    private Integer threadCount;

    private String jmxConfig;

    private Properties props;

    /**
     * Instantiates a new Metrics config.
     *
     * @param metricsName the metrics name
     * @param host        the host
     * @param port        the port
     * @param async       the async
     * @param threadCount the thread count
     * @param jmxConfig   the jmx config
     * @param props       the props
     */
    public MetricsConfig(final String metricsName, final String host, final Integer port, final Boolean async, final Integer threadCount, final String jmxConfig, final Properties props) {
        this.metricsName = metricsName;
        this.host = host;
        this.port = port;
        this.async = async;
        this.threadCount = threadCount;
        this.jmxConfig = jmxConfig;
        this.props = props;
    }

    /**
     * Gets metrics name.
     *
     * @return the metrics name
     */
    public String getMetricsName() {
        return metricsName;
    }

    /**
     * Sets metrics name.
     *
     * @param metricsName the metrics name
     * @return the metrics name
     */
    public MetricsConfig setMetricsName(final String metricsName) {
        this.metricsName = metricsName;
        return this;
    }

    /**
     * Gets host.
     *
     * @return the host
     */
    public String getHost() {
        return host;
    }

    /**
     * Sets host.
     *
     * @param host the host
     * @return the host
     */
    public MetricsConfig setHost(final String host) {
        this.host = host;
        return this;
    }

    /**
     * Gets port.
     *
     * @return the port
     */
    public Integer getPort() {
        return port;
    }

    /**
     * Sets port.
     *
     * @param port the port
     * @return the port
     */
    public MetricsConfig setPort(final Integer port) {
        this.port = port;
        return this;
    }

    /**
     * Gets async.
     *
     * @return the async
     */
    public Boolean getAsync() {
        return async;
    }

    /**
     * Sets async.
     *
     * @param async the async
     * @return the async
     */
    public MetricsConfig setAsync(final Boolean async) {
        this.async = async;
        return this;
    }

    /**
     * Gets thread count.
     *
     * @return the thread count
     */
    public Integer getThreadCount() {
        return threadCount;
    }

    /**
     * Sets thread count.
     *
     * @param threadCount the thread count
     * @return the thread count
     */
    public MetricsConfig setThreadCount(final Integer threadCount) {
        this.threadCount = threadCount;
        return this;
    }

    /**
     * Gets jmx config.
     *
     * @return the jmx config
     */
    public String getJmxConfig() {
        return jmxConfig;
    }

    /**
     * Sets jmx config.
     *
     * @param jmxConfig the jmx config
     * @return the jmx config
     */
    public MetricsConfig setJmxConfig(final String jmxConfig) {
        this.jmxConfig = jmxConfig;
        return this;
    }

    /**
     * Gets props.
     *
     * @return the props
     */
    public Properties getProps() {
        return props;
    }

    /**
     * Sets props.
     *
     * @param props the props
     * @return the props
     */
    public MetricsConfig setProps(final Properties props) {
        this.props = props;
        return this;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        MetricsConfig that = (MetricsConfig) o;

        if (!Objects.equals(metricsName, that.metricsName)) {
            return false;
        }
        if (!Objects.equals(host, that.host)) {
            return false;
        }
        if (!Objects.equals(port, that.port)) {
            return false;
        }
        if (!Objects.equals(async, that.async)) {
            return false;
        }
        if (!Objects.equals(threadCount, that.threadCount)) {
            return false;
        }
        if (!Objects.equals(jmxConfig, that.jmxConfig)) {
            return false;
        }
        return Objects.equals(props, that.props);
    }

    @Override
    public int hashCode() {
        int result = metricsName != null ? metricsName.hashCode() : 0;
        result = 31 * result + (host != null ? host.hashCode() : 0);
        result = 31 * result + (port != null ? port.hashCode() : 0);
        result = 31 * result + (async != null ? async.hashCode() : 0);
        result = 31 * result + (threadCount != null ? threadCount.hashCode() : 0);
        result = 31 * result + (jmxConfig != null ? jmxConfig.hashCode() : 0);
        result = 31 * result + (props != null ? props.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", MetricsConfig.class.getSimpleName() + "(", ")")
                .add("metricsName=" + metricsName)
                .add("host=" + host)
                .add("port=" + port)
                .add("async=" + async)
                .add("threadCount=" + threadCount)
                .add("jmxConfig=" + jmxConfig)
                .add("props=" + props)
                .toString();
    }
}

