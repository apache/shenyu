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

package org.apache.shenyu.plugin.httpclient.config;

import org.springframework.boot.web.server.WebServerException;
import org.springframework.util.ResourceUtils;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.tcp.SslProvider;

import java.io.IOException;
import java.net.URL;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Configuration properties for the Netty {@link reactor.netty.http.client.HttpClient}.
 */
public class HttpClientProperties {

    /**
     * The connect timeout in millis, the default is 45s.
     */
    private Integer connectTimeout;

    /**
     * The response timeout.
     */
    private Duration responseTimeout;

    /**
     * readTimeout, the default is 3s.
     */
    private Integer readTimeout = 3000;

    /**
     * writeTimeout, the default is 3s.
     */
    private Integer writeTimeout = 3000;

    /**
     * Pool configuration for Netty HttpClient.
     */
    private Pool pool = new Pool();

    /**
     * Proxy configuration for Netty HttpClient.
     */
    private Proxy proxy = new Proxy();

    /**
     * SSL configuration for Netty HttpClient.
     */
    private Ssl ssl = new Ssl();

    /**
     * Enables wiretap debugging for Netty HttpClient.
     */
    private boolean wiretap;

    /**
     * Gets connect timeout.
     *
     * @return the connect timeout
     */
    public Integer getConnectTimeout() {
        return connectTimeout;
    }

    /**
     * Sets connect timeout.
     *
     * @param connectTimeout the connect timeout
     */
    public void setConnectTimeout(final Integer connectTimeout) {
        this.connectTimeout = connectTimeout;
    }

    /**
     * Gets response timeout.
     *
     * @return the response timeout
     */
    public Duration getResponseTimeout() {
        return responseTimeout;
    }

    /**
     * Sets response timeout.
     *
     * @param responseTimeout the response timeout
     */
    public void setResponseTimeout(final Duration responseTimeout) {
        this.responseTimeout = responseTimeout;
    }

    /**
     * Gets read timeout.
     *
     * @return the read timeout
     */
    public Integer getReadTimeout() {
        return readTimeout;
    }

    /**
     * Sets read timeout.
     *
     * @param readTimeout the read timeout
     */
    public void setReadTimeout(final Integer readTimeout) {
        this.readTimeout = readTimeout;
    }

    /**
     * Gets write timeout.
     *
     * @return the write timeout
     */
    public Integer getWriteTimeout() {
        return writeTimeout;
    }

    /**
     * Sets write timeout.
     *
     * @param writeTimeout the write timeout
     */
    public void setWriteTimeout(final Integer writeTimeout) {
        this.writeTimeout = writeTimeout;
    }

    /**
     * Gets pool.
     *
     * @return the pool
     */
    public Pool getPool() {
        return pool;
    }

    /**
     * Sets pool.
     *
     * @param pool the pool
     */
    public void setPool(final Pool pool) {
        this.pool = pool;
    }

    /**
     * Gets proxy.
     *
     * @return the proxy
     */
    public Proxy getProxy() {
        return proxy;
    }

    /**
     * Sets proxy.
     *
     * @param proxy the proxy
     */
    public void setProxy(final Proxy proxy) {
        this.proxy = proxy;
    }

    /**
     * Gets ssl.
     *
     * @return the ssl
     */
    public Ssl getSsl() {
        return ssl;
    }

    /**
     * Sets ssl.
     *
     * @param ssl the ssl
     */
    public void setSsl(final Ssl ssl) {
        this.ssl = ssl;
    }

    /**
     * Is wiretap boolean.
     *
     * @return the boolean
     */
    public boolean isWiretap() {
        return wiretap;
    }

    /**
     * Sets wiretap.
     *
     * @param wiretap the wiretap
     */
    public void setWiretap(final boolean wiretap) {
        this.wiretap = wiretap;
    }

    /**
     * The type Pool.
     */
    public static class Pool {

        /**
         * Type of pool for HttpClient to use, defaults to ELASTIC.
         */
        private PoolType type = PoolType.ELASTIC;

        /**
         * The channel pool map name, defaults to proxy.
         */
        private String name = "proxy";

        /**
         * Only for type FIXED, the maximum number of connections before starting pending
         * acquisition on existing ones.
         */
        private Integer maxConnections = ConnectionProvider.DEFAULT_POOL_MAX_CONNECTIONS;

        /**
         * Only for type FIXED, the maximum time in millis to wait for aquiring.
         */
        private Long acquireTimeout = ConnectionProvider.DEFAULT_POOL_ACQUIRE_TIMEOUT;

        /**
         * Gets type.
         *
         * @return the type
         */
        public PoolType getType() {
            return type;
        }

        /**
         * Sets type.
         *
         * @param type the type
         */
        public void setType(final PoolType type) {
            this.type = type;
        }

        /**
         * Gets name.
         *
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * Sets name.
         *
         * @param name the name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * Gets max connections.
         *
         * @return the max connections
         */
        public Integer getMaxConnections() {
            return maxConnections;
        }

        /**
         * Sets max connections.
         *
         * @param maxConnections the max connections
         */
        public void setMaxConnections(final Integer maxConnections) {
            this.maxConnections = maxConnections;
        }

        /**
         * Gets acquire timeout.
         *
         * @return the acquire timeout
         */
        public Long getAcquireTimeout() {
            return acquireTimeout;
        }

        /**
         * Sets acquire timeout.
         *
         * @param acquireTimeout the acquire timeout
         */
        public void setAcquireTimeout(final Long acquireTimeout) {
            this.acquireTimeout = acquireTimeout;
        }

        /**
         * The enum Pool type.
         */
        public enum PoolType {

            /**
             * Elastic pool type.
             */
            ELASTIC,

            /**
             * Fixed pool type.
             */
            FIXED,

            /**
             * Disabled pool type.
             */
            DISABLED
        }
    }

    /**
     * The type Proxy.
     */
    public static class Proxy {

        /**
         * Hostname for proxy configuration of Netty HttpClient.
         */
        private String host;

        /**
         * Port for proxy configuration of Netty HttpClient.
         */
        private Integer port;

        /**
         * Username for proxy configuration of Netty HttpClient.
         */
        private String username;

        /**
         * Password for proxy configuration of Netty HttpClient.
         */
        private String password;

        /**
         * Regular expression (Java) for a configured list of hosts. that should be
         * reached directly, bypassing the proxy
         */
        private String nonProxyHostsPattern;

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
         */
        public void setHost(final String host) {
            this.host = host;
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
         */
        public void setPort(final Integer port) {
            this.port = port;
        }

        /**
         * Gets username.
         *
         * @return the username
         */
        public String getUsername() {
            return username;
        }

        /**
         * Sets username.
         *
         * @param username the username
         */
        public void setUsername(final String username) {
            this.username = username;
        }

        /**
         * Gets password.
         *
         * @return the password
         */
        public String getPassword() {
            return password;
        }

        /**
         * Sets password.
         *
         * @param password the password
         */
        public void setPassword(final String password) {
            this.password = password;
        }

        /**
         * Gets non proxy hosts pattern.
         *
         * @return the non proxy hosts pattern
         */
        public String getNonProxyHostsPattern() {
            return nonProxyHostsPattern;
        }

        /**
         * Sets non proxy hosts pattern.
         *
         * @param nonProxyHostsPattern the non proxy hosts pattern
         */
        public void setNonProxyHostsPattern(final String nonProxyHostsPattern) {
            this.nonProxyHostsPattern = nonProxyHostsPattern;
        }
    }

    /**
     * The type Ssl.
     */
    public class Ssl {

        /**
         * Installs the netty InsecureTrustManagerFactory. This is insecure and not
         * suitable for production.
         */
        private boolean useInsecureTrustManager;

        /**
         * Trusted certificates for verifying the remote endpoint's certificate.
         */
        private List<String> trustedX509Certificates = new ArrayList<>();

        // use netty default SSL timeouts
        /**
         * SSL handshake timeout. Default to 10000 ms
         */
        private Duration handshakeTimeout = Duration.ofMillis(10000);

        /**
         * SSL close_notify flush timeout. Default to 3000 ms.
         */
        private Duration closeNotifyFlushTimeout = Duration.ofMillis(3000);

        /**
         * SSL close_notify read timeout. Default to 0 ms.
         */
        private Duration closeNotifyReadTimeout = Duration.ZERO;

        /**
         * The default ssl configuration type. Defaults to TCP.
         */
        private SslProvider.DefaultConfigurationType defaultConfigurationType = SslProvider.DefaultConfigurationType.TCP;

        /**
         * Is use insecure trust manager boolean.
         *
         * @return the boolean
         */
        public boolean isUseInsecureTrustManager() {
            return useInsecureTrustManager;
        }

        /**
         * Sets use insecure trust manager.
         *
         * @param useInsecureTrustManager the use insecure trust manager
         */
        public void setUseInsecureTrustManager(final boolean useInsecureTrustManager) {
            this.useInsecureTrustManager = useInsecureTrustManager;
        }

        /**
         * Gets trusted x 509 certificates.
         *
         * @return the trusted x 509 certificates
         */
        public List<String> getTrustedX509Certificates() {
            return trustedX509Certificates;
        }

        /**
         * Sets trusted x 509 certificates.
         *
         * @param trustedX509Certificates the trusted x 509 certificates
         */
        public void setTrustedX509Certificates(final List<String> trustedX509Certificates) {
            this.trustedX509Certificates = trustedX509Certificates;
        }

        /**
         * Gets handshake timeout.
         *
         * @return the handshake timeout
         */
        public Duration getHandshakeTimeout() {
            return handshakeTimeout;
        }

        /**
         * Sets handshake timeout.
         *
         * @param handshakeTimeout the handshake timeout
         */
        public void setHandshakeTimeout(final Duration handshakeTimeout) {
            this.handshakeTimeout = handshakeTimeout;
        }

        /**
         * Gets close notify flush timeout.
         *
         * @return the close notify flush timeout
         */
        public Duration getCloseNotifyFlushTimeout() {
            return closeNotifyFlushTimeout;
        }

        /**
         * Sets close notify flush timeout.
         *
         * @param closeNotifyFlushTimeout the close notify flush timeout
         */
        public void setCloseNotifyFlushTimeout(final Duration closeNotifyFlushTimeout) {
            this.closeNotifyFlushTimeout = closeNotifyFlushTimeout;
        }

        /**
         * Gets close notify read timeout.
         *
         * @return the close notify read timeout
         */
        public Duration getCloseNotifyReadTimeout() {
            return closeNotifyReadTimeout;
        }

        /**
         * Sets close notify read timeout.
         *
         * @param closeNotifyReadTimeout the close notify read timeout
         */
        public void setCloseNotifyReadTimeout(final Duration closeNotifyReadTimeout) {
            this.closeNotifyReadTimeout = closeNotifyReadTimeout;
        }

        /**
         * Gets default configuration type.
         *
         * @return the default configuration type
         */
        public SslProvider.DefaultConfigurationType getDefaultConfigurationType() {
            return defaultConfigurationType;
        }

        /**
         * Sets default configuration type.
         *
         * @param defaultConfigurationType the default configuration type
         */
        public void setDefaultConfigurationType(final SslProvider.DefaultConfigurationType defaultConfigurationType) {
            this.defaultConfigurationType = defaultConfigurationType;
        }

        /**
         * Get trusted x 509 certificates for trust manager x 509 certificate [ ].
         *
         * @return the x 509 certificate [ ]
         */
        public X509Certificate[] getTrustedX509CertificatesForTrustManager() {
            try {
                CertificateFactory certificateFactory = CertificateFactory
                        .getInstance("X.509");
                List<Certificate> allCerts = new ArrayList<>();
                for (String trustedCert : ssl.getTrustedX509Certificates()) {
                    try {
                        URL url = ResourceUtils.getURL(trustedCert);
                        Collection<? extends Certificate> certs = certificateFactory
                                .generateCertificates(url.openStream());
                        allCerts.addAll(certs);
                    } catch (IOException e) {
                        throw new WebServerException(
                                "Could not load certificate '" + trustedCert + "'", e);
                    }
                }
                X509Certificate[] x509Certificates = new X509Certificate[allCerts.size()];
                return allCerts.toArray(x509Certificates);
            } catch (CertificateException e) {
                throw new WebServerException("Could not load CertificateFactory X.509", e);
            }
        }
    }
}
