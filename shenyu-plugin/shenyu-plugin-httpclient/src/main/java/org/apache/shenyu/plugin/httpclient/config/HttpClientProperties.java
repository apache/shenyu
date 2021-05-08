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

import lombok.Data;
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
@Data
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
     * The type Pool.
     */
    @Data
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
    @Data
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
    }

    /**
     * The type Ssl.
     */
    @Data
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
