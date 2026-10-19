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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

import java.time.Duration;
import java.util.Objects;

/**
 * AI proxy transport properties.
 */
public class AiProxyTransportProperties {

    /**
     * Maximum duration allowed for establishing an upstream connection.
     */
    private Duration connectTimeout = Duration.ofSeconds(30);

    /**
     * Maximum duration allowed for receiving upstream response headers.
     */
    private Duration responseHeaderTimeout = Duration.ofSeconds(30);

    /**
     * Maximum idle duration allowed between upstream response body chunks.
     */
    private Duration streamingIdleTimeout = Duration.ofSeconds(60);

    /**
     * Maximum total duration allowed for an upstream response body stream.
     */
    private Duration totalStreamDuration = Duration.ofMinutes(10);

    /**
     * Maximum total bytes allowed for one upstream response. Zero means unlimited.
     */
    private long maxResponseBytes;

    private final Pool pool = new Pool();

    private final Tls tls = new Tls();

    private final Proxy proxy = new Proxy();

    /**
     * Get connection pool properties.
     *
     * @return connection pool properties
     */
    public Pool getPool() {
        return pool;
    }

    /**
     * Get TLS properties.
     *
     * @return TLS properties
     */
    public Tls getTls() {
        return tls;
    }

    /**
     * Get HTTP proxy properties.
     *
     * @return HTTP proxy properties
     */
    public Proxy getProxy() {
        return proxy;
    }

    /**
     * Get connect timeout.
     *
     * @return connect timeout
     */
    public Duration getConnectTimeout() {
        return connectTimeout;
    }

    /**
     * Set connect timeout.
     *
     * @param connectTimeout connect timeout
     */
    public void setConnectTimeout(final Duration connectTimeout) {
        this.connectTimeout = requirePositive(connectTimeout, "connectTimeout");
    }

    /**
     * Get response header timeout.
     *
     * @return response header timeout
     */
    public Duration getResponseHeaderTimeout() {
        return responseHeaderTimeout;
    }

    /**
     * Set response header timeout.
     *
     * @param responseHeaderTimeout response header timeout
     */
    public void setResponseHeaderTimeout(final Duration responseHeaderTimeout) {
        this.responseHeaderTimeout = requirePositive(responseHeaderTimeout, "responseHeaderTimeout");
    }

    /**
     * Get streaming idle timeout.
     *
     * @return streaming idle timeout
     */
    public Duration getStreamingIdleTimeout() {
        return streamingIdleTimeout;
    }

    /**
     * Set streaming idle timeout.
     *
     * @param streamingIdleTimeout streaming idle timeout
     */
    public void setStreamingIdleTimeout(final Duration streamingIdleTimeout) {
        this.streamingIdleTimeout = requirePositive(streamingIdleTimeout, "streamingIdleTimeout");
    }

    /**
     * Get total stream duration.
     *
     * @return total stream duration
     */
    public Duration getTotalStreamDuration() {
        return totalStreamDuration;
    }

    /**
     * Set total stream duration.
     *
     * @param totalStreamDuration total stream duration
     */
    public void setTotalStreamDuration(final Duration totalStreamDuration) {
        this.totalStreamDuration = requirePositive(totalStreamDuration, "totalStreamDuration");
    }

    /**
     * Get maximum upstream response bytes.
     *
     * @return maximum upstream response bytes, or zero when unlimited
     */
    public long getMaxResponseBytes() {
        return maxResponseBytes;
    }

    /**
     * Set maximum upstream response bytes.
     *
     * @param maxResponseBytes maximum upstream response bytes, or zero when unlimited
     */
    public void setMaxResponseBytes(final long maxResponseBytes) {
        this.maxResponseBytes = requireNonNegative(maxResponseBytes, "maxResponseBytes");
    }

    static Duration requirePositive(final Duration duration, final String name) {
        Objects.requireNonNull(duration, name + " must not be null");
        if (duration.isZero() || duration.isNegative()) {
            throw new IllegalArgumentException(name + " must be greater than zero");
        }
        return duration;
    }

    private static int requirePositive(final int value, final String name) {
        if (value <= 0) {
            throw new IllegalArgumentException(name + " must be greater than zero");
        }
        return value;
    }

    static long requireNonNegative(final long value, final String name) {
        if (value < 0) {
            throw new IllegalArgumentException(name + " must be greater than or equal to zero");
        }
        return value;
    }

    /**
     * AI proxy connection pool properties.
     */
    public static class Pool {
        private boolean enabled = true;

        private String name = "shenyu-ai-proxy";

        private int maxConnections = 30;

        private int pendingAcquireMaxCount = 60;

        private Duration pendingAcquireTimeout = Duration.ofSeconds(3);

        private Duration maxIdleTime = Duration.ofSeconds(60);

        private Duration maxLifeTime = Duration.ofMinutes(30);

        private Duration evictionInterval = Duration.ofSeconds(30);

        private Duration gracefulDisposeTimeout = Duration.ofSeconds(30);

        /**
         * Whether connection pooling is enabled.
         *
         * @return true when connection pooling is enabled
         */
        public boolean isEnabled() {
            return enabled;
        }

        /**
         * Set whether connection pooling is enabled.
         *
         * @param enabled whether connection pooling is enabled
         */
        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }

        /**
         * Get the connection provider name.
         *
         * @return connection provider name
         */
        public String getName() {
            return name;
        }

        /**
         * Set the connection provider name.
         *
         * @param name connection provider name
         */
        public void setName(final String name) {
            Objects.requireNonNull(name, "name must not be null");
            if (name.isBlank()) {
                throw new IllegalArgumentException("name must not be blank");
            }
            this.name = name;
        }

        /**
         * Get maximum connections per connection pool.
         *
         * @return maximum connections
         */
        public int getMaxConnections() {
            return maxConnections;
        }

        /**
         * Set maximum connections per connection pool.
         *
         * @param maxConnections maximum connections
         */
        public void setMaxConnections(final int maxConnections) {
            this.maxConnections = requirePositive(maxConnections, "maxConnections");
        }

        /**
         * Get maximum pending connection acquires.
         *
         * @return maximum pending connection acquires
         */
        public int getPendingAcquireMaxCount() {
            return pendingAcquireMaxCount;
        }

        /**
         * Set maximum pending connection acquires. Minus one means unbounded.
         *
         * @param pendingAcquireMaxCount maximum pending connection acquires
         */
        public void setPendingAcquireMaxCount(final int pendingAcquireMaxCount) {
            if (pendingAcquireMaxCount != -1 && pendingAcquireMaxCount <= 0) {
                throw new IllegalArgumentException("pendingAcquireMaxCount must be -1 or greater than zero");
            }
            this.pendingAcquireMaxCount = pendingAcquireMaxCount;
        }

        /**
         * Get pending connection acquire timeout.
         *
         * @return pending connection acquire timeout
         */
        public Duration getPendingAcquireTimeout() {
            return pendingAcquireTimeout;
        }

        /**
         * Set pending connection acquire timeout.
         *
         * @param pendingAcquireTimeout pending connection acquire timeout
         */
        public void setPendingAcquireTimeout(final Duration pendingAcquireTimeout) {
            this.pendingAcquireTimeout = requirePositive(pendingAcquireTimeout, "pendingAcquireTimeout");
        }

        /**
         * Get maximum idle time.
         *
         * @return maximum idle time
         */
        public Duration getMaxIdleTime() {
            return maxIdleTime;
        }

        /**
         * Set maximum idle time.
         *
         * @param maxIdleTime maximum idle time
         */
        public void setMaxIdleTime(final Duration maxIdleTime) {
            this.maxIdleTime = requirePositive(maxIdleTime, "maxIdleTime");
        }

        /**
         * Get maximum connection lifetime.
         *
         * @return maximum connection lifetime
         */
        public Duration getMaxLifeTime() {
            return maxLifeTime;
        }

        /**
         * Set maximum connection lifetime.
         *
         * @param maxLifeTime maximum connection lifetime
         */
        public void setMaxLifeTime(final Duration maxLifeTime) {
            this.maxLifeTime = requirePositive(maxLifeTime, "maxLifeTime");
        }

        /**
         * Get background eviction interval.
         *
         * @return background eviction interval
         */
        public Duration getEvictionInterval() {
            return evictionInterval;
        }

        /**
         * Set background eviction interval.
         *
         * @param evictionInterval background eviction interval
         */
        public void setEvictionInterval(final Duration evictionInterval) {
            this.evictionInterval = requirePositive(evictionInterval, "evictionInterval");
        }

        /**
         * Get graceful disposal timeout.
         *
         * @return graceful disposal timeout
         */
        public Duration getGracefulDisposeTimeout() {
            return gracefulDisposeTimeout;
        }

        /**
         * Set graceful disposal timeout.
         *
         * @param gracefulDisposeTimeout graceful disposal timeout
         */
        public void setGracefulDisposeTimeout(final Duration gracefulDisposeTimeout) {
            this.gracefulDisposeTimeout = requirePositive(gracefulDisposeTimeout, "gracefulDisposeTimeout");
        }
    }

    /**
     * AI proxy TLS properties.
     */
    public static class Tls {
        private boolean verify = true;

        /**
         * Whether upstream TLS certificates are verified.
         *
         * @return true when upstream TLS certificates are verified
         */
        public boolean isVerify() {
            return verify;
        }

        /**
         * Set whether upstream TLS certificates are verified.
         *
         * @param verify whether upstream TLS certificates are verified
         */
        public void setVerify(final boolean verify) {
            this.verify = verify;
        }
    }

    /**
     * AI proxy HTTP forward proxy properties.
     */
    public static class Proxy {
        private boolean enabled;

        private String host;

        private int port;

        private String username;

        private String password;

        private String nonProxyHostsPattern;

        /**
         * Whether the HTTP forward proxy is enabled.
         *
         * @return true when the HTTP forward proxy is enabled
         */
        public boolean isEnabled() {
            return enabled;
        }

        /**
         * Set whether the HTTP forward proxy is enabled.
         *
         * @param enabled whether the HTTP forward proxy is enabled
         */
        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }

        /**
         * Get HTTP proxy host.
         *
         * @return HTTP proxy host
         */
        public String getHost() {
            return host;
        }

        /**
         * Set HTTP proxy host.
         *
         * @param host HTTP proxy host
         */
        public void setHost(final String host) {
            this.host = host;
        }

        /**
         * Get HTTP proxy port.
         *
         * @return HTTP proxy port
         */
        public int getPort() {
            return port;
        }

        /**
         * Set HTTP proxy port.
         *
         * @param port HTTP proxy port
         */
        public void setPort(final int port) {
            if (port < 1 || port > 65535) {
                throw new IllegalArgumentException("port must be between 1 and 65535");
            }
            this.port = port;
        }

        /**
         * Get HTTP proxy username.
         *
         * @return HTTP proxy username
         */
        public String getUsername() {
            return username;
        }

        /**
         * Set HTTP proxy username.
         *
         * @param username HTTP proxy username
         */
        public void setUsername(final String username) {
            this.username = username;
        }

        /**
         * Get HTTP proxy password.
         *
         * @return HTTP proxy password
         */
        public String getPassword() {
            return password;
        }

        /**
         * Set HTTP proxy password.
         *
         * @param password HTTP proxy password
         */
        public void setPassword(final String password) {
            this.password = password;
        }

        /**
         * Get hosts pattern that bypasses the HTTP proxy.
         *
         * @return non-proxy hosts pattern
         */
        public String getNonProxyHostsPattern() {
            return nonProxyHostsPattern;
        }

        /**
         * Set hosts pattern that bypasses the HTTP proxy.
         *
         * @param nonProxyHostsPattern non-proxy hosts pattern
         */
        public void setNonProxyHostsPattern(final String nonProxyHostsPattern) {
            this.nonProxyHostsPattern = nonProxyHostsPattern;
        }
    }

}
