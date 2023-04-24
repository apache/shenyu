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

import io.netty.handler.ssl.SslProvider;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.springframework.util.ResourceUtils;
import reactor.netty.ReactorNetty;
import reactor.netty.resources.ConnectionProvider;
import reactor.netty.resources.LoopResources;

import javax.net.ssl.KeyManagerFactory;
import java.io.IOException;
import java.net.URL;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchProviderException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * Configuration properties for the Netty {@link reactor.netty.http.client.HttpClient}.
 */
public class HttpClientProperties {

    /**
     * the http client strategy.
     */
    private String strategy;

    /**
     * The connection timeout in millis, the default is 45s.
     */
    private Integer connectTimeout = 45000;

    /**
     * The response timeout.
     */
    private Long responseTimeout = 3000L;

    /**
     * readerIdleTime, the default is 3s.
     */
    private Integer readerIdleTime = 3000;

    /**
     * writerIdleTime, the default is 3s.
     */
    private Integer writerIdleTime = 3000;

    /**
     * allIdleTime, the default is 3s.
     */
    private Integer allIdleTime = 3000;

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
     * ThreadPool configuration for Netty HttpClient.
     */
    private ThreadPool threadPool = new ThreadPool();

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
     * set to false, fix java.io.IOException: Connection reset by peer, see https://github.com/reactor/reactor-netty/issues/388.
     */
    private boolean keepAlive;

    /**
     * body max memory size, unit mb.
     */
    private Integer maxInMemorySize = 1;

    /**
     * Gets strategy.
     *
     * @return to strategy
     */
    public String getStrategy() {
        return strategy;
    }

    /**
     * Sets strategy.
     *
     * @param strategy to strategy
     */
    public void setStrategy(final String strategy) {
        this.strategy = strategy;
    }

    /**
     * Gets connect timeout.
     *
     * @return to connect timeout
     */
    public Integer getConnectTimeout() {
        return connectTimeout;
    }
    
    /**
     * Sets connect timeout.
     *
     * @param connectTimeout to connect timeout
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
        return Optional.ofNullable(responseTimeout)
                .map(it -> Duration.ofMillis(responseTimeout)).orElse(Duration.ofMillis(3000));
    }
    
    /**
     * Sets response timeout.
     *
     * @param responseTimeout the response timeout
     */
    public void setResponseTimeout(final Long responseTimeout) {
        this.responseTimeout = responseTimeout;
    }
    
    /**
     * Gets read idle timeout.
     *
     * @return the read idle timeout
     */
    public Integer getReaderIdleTime() {
        return readerIdleTime;
    }
    
    /**
     * Sets read idle timeout.
     *
     * @param readerIdleTime the read idle timeout
     */
    public void setReaderIdleTime(final Integer readerIdleTime) {
        this.readerIdleTime = readerIdleTime;
    }
    
    /**
     * Gets writer idle timeout.
     *
     * @return the writer idle timeout
     */
    public Integer getWriterIdleTime() {
        return writerIdleTime;
    }
    
    /**
     * Sets writer idle timeout.
     *
     * @param writerIdleTime the writer idle timeout
     */
    public void setWriterIdleTime(final Integer writerIdleTime) {
        this.writerIdleTime = writerIdleTime;
    }
    
    /**
     * Gets all idle timeout.
     *
     * @return the all idle timeout
     */
    public Integer getAllIdleTime() {
        return allIdleTime;
    }
    
    /**
     * Sets all idle timeout.
     *
     * @param allIdleTime the all idle timeout
     */
    public void setAllIdleTime(final Integer allIdleTime) {
        this.allIdleTime = allIdleTime;
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
     * Gets thread pool.
     *
     * @return the thread pool
     */
    public ThreadPool getThreadPool() {
        return threadPool;
    }

    /**
     * Sets thread pool.
     *
     * @param threadPool the thread pool
     */
    public void setThreadPool(final ThreadPool threadPool) {
        this.threadPool = threadPool;
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
     * Is keepAlive boolean.
     *
     * @return the boolean
     */
    public boolean isKeepAlive() {
        return keepAlive;
    }

    /**
     * Sets keepAlive.
     *
     * @param keepAlive the keepAlive
     */
    public void setKeepAlive(final boolean keepAlive) {
        this.keepAlive = keepAlive;
    }

    /**
     * get maxInMemorySize.
     * @return maxInMemorySize
     */
    public Integer getMaxInMemorySize() {
        return maxInMemorySize;
    }

    /**
     * set maxInMemorySize.
     * @param maxInMemorySize maxInMemorySize
     */
    public void setMaxInMemorySize(final Integer maxInMemorySize) {
        this.maxInMemorySize = maxInMemorySize;
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
         * Time in millis after which the channel will be closed,
         * if NULL there is no max idle time.
         */
        private Long maxIdleTime;

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
         * Gets maxIdleTime timeout.
         *
         * @return the maxIdleTime timeout
         */
        public Duration getMaxIdleTime() {
            return Optional.ofNullable(maxIdleTime)
                    .map(it -> Duration.ofMillis(maxIdleTime)).orElse(Duration.ofMillis(3000L));
        }

        /**
         * Sets maxIdleTime timeout.
         *
         * @param maxIdleTime the maxIdleTime timeout
         */
        public void setMaxIdleTime(final Long maxIdleTime) {
            this.maxIdleTime = maxIdleTime;
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
     * The type Thread Pool.
     */
    public static class ThreadPool {

        /**
         * The the event loop thread name prefix, defaults to shenyu.
         */
        private String prefix = "shenyu";

        /**
         * The selector thread count, defaults to 1.
         */
        private Integer selectCount = Integer.parseInt(System.getProperty(ReactorNetty.IO_SELECT_COUNT, "1"));

        /**
         * The worker thread count, defaults to available processor (but with a minimum value of 4).
         */
        private Integer workerCount = LoopResources.DEFAULT_IO_WORKER_COUNT;

        /**
         * Whether the thread created by the thread pool is a daemon thread.
         */
        private Boolean daemon = true;

        /**
         * Gets prefix.
         *
         * @return the prefix
         */
        public String getPrefix() {
            return prefix;
        }

        /**
         * Sets prefix.
         *
         * @param prefix the prefix
         */
        public void setPrefix(final String prefix) {
            this.prefix = prefix;
        }

        /**
         * Gets select count.
         *
         * @return the select count
         */
        public Integer getSelectCount() {
            return selectCount;
        }

        /**
         * Sets select count.
         *
         * @param selectCount the select count
         */
        public void setSelectCount(final Integer selectCount) {
            this.selectCount = selectCount;
        }

        /**
         * Gets worker count.
         *
         * @return the worker count
         */
        public Integer getWorkerCount() {
            return workerCount;
        }

        /**
         * Sets worker count.
         *
         * @param workerCount the worker count
         */
        public void setWorkerCount(final Integer workerCount) {
            this.workerCount = workerCount;
        }

        /**
         * Gets daemon.
         *
         * @return the daemon
         */
        public Boolean getDaemon() {
            return daemon;
        }

        /**
         * Sets daemon.
         *
         * @param daemon the daemon
         */
        public void setDaemon(final Boolean daemon) {
            this.daemon = daemon;
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
    
        private String keyStorePath;
    
        /**
         * Keystore type for netty httpClient and webclient, default is PKCS12. 
         */
        private String keyStoreType = "PKCS12";
    
        /** 
         * Keystore provider for netty httpClient and webclient.
         */
        private String keyStoreProvider;
    
        /** 
         * Keystore password.
         */
        private String keyStorePassword;
    
        /** 
         * Key password ,this is same to keyStorePassword.
         */
        private String keyPassword;

        /**
         * The default ssl configuration type. Defaults to JDK Provider.
         */
        private SslProvider defaultConfigurationType = SslProvider.JDK;
    
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
        public SslProvider getDefaultConfigurationType() {
            return defaultConfigurationType;
        }
    
        /**
         * Sets default configuration type.
         *
         * @param defaultConfigurationType the default configuration type
         */
        public void setDefaultConfigurationType(final SslProvider defaultConfigurationType) {
            this.defaultConfigurationType = defaultConfigurationType;
        }
    
        /**
         * Gets key store path.
         *
         * @return the key store path
         */
        public String getKeyStorePath() {
            return keyStorePath;
        }
    
        /**
         * Sets key store path.
         *
         * @param keyStorePath the key store path
         */
        public void setKeyStorePath(final String keyStorePath) {
            this.keyStorePath = keyStorePath;
        }
    
        /**
         * Gets key store type.
         *
         * @return the key store type
         */
        public String getKeyStoreType() {
            return keyStoreType;
        }
    
        /**
         * Sets key store type.
         *
         * @param keyStoreType the key store type
         */
        public void setKeyStoreType(final String keyStoreType) {
            this.keyStoreType = keyStoreType;
        }
    
        /**
         * Gets key store provider.
         *
         * @return the key store provider
         */
        public String getKeyStoreProvider() {
            return keyStoreProvider;
        }
    
        /**
         * Sets key store provider.
         *
         * @param keyStoreProvider the key store provider
         */
        public void setKeyStoreProvider(final String keyStoreProvider) {
            this.keyStoreProvider = keyStoreProvider;
        }
    
        /**
         * Gets key store password.
         *
         * @return the key store password
         */
        public String getKeyStorePassword() {
            return keyStorePassword;
        }
    
        /**
         * Sets key store password.
         *
         * @param keyStorePassword the key store password
         */
        public void setKeyStorePassword(final String keyStorePassword) {
            this.keyStorePassword = keyStorePassword;
        }
    
        /**
         * Gets key password.
         *
         * @return the key password
         */
        public String getKeyPassword() {
            return keyPassword;
        }
    
        /**
         * Sets key password.
         *
         * @param keyPassword the key password
         */
        public void setKeyPassword(final String keyPassword) {
            this.keyPassword = keyPassword;
        }
    
        /**
         * Get trusted x 509 certificates for trust manager x 509 certificate [].
         *
         * @return the x 509 certificate []
         */
        @SuppressWarnings("all")
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
                        throw new ShenyuException(
                                "Could not load certificate '" + trustedCert + "'", e);
                    }
                }
                return allCerts.toArray(new X509Certificate[allCerts.size()]);
            } catch (CertificateException e) {
                throw new ShenyuException("Could not load CertificateFactory X.509", e);
            }
        }
    
        /**
         * Gets key manager factory.
         *
         * @return the key manager factory
         */
        public KeyManagerFactory getKeyManagerFactory() {
            try {
                if (StringUtils.isNotEmpty(getKeyStorePath())) {
                    KeyManagerFactory keyManagerFactory = KeyManagerFactory
                            .getInstance(KeyManagerFactory.getDefaultAlgorithm());
                    char[] keyPassword = Optional.ofNullable(getKeyPassword())
                            .map(String::toCharArray).orElse(getKeyStorePassword().toCharArray());
                    keyManagerFactory.init(this.createKeyStore(), keyPassword);
                    return keyManagerFactory;
                }
                return null;
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        }
    
        /**
         * Create key store.
         *
         * @return the key store
         */
        public KeyStore createKeyStore() {
            String provider = getKeyStoreProvider();
            String storeType = getKeyStoreType();
            String keyStorePath = getKeyStorePath();
            String keyStorePassword = getKeyStorePassword();
            try {
                KeyStore keyStore = StringUtils.isNotEmpty(provider)
                        ? KeyStore.getInstance(storeType, provider)
                        : KeyStore.getInstance(storeType);
                try {
                    char[] keyPassword = Optional.ofNullable(keyStorePassword)
                            .map(String::toCharArray).orElse(null);
                    URL url = ResourceUtils.getURL(keyStorePath);
                    keyStore.load(url.openStream(), keyPassword);
                } catch (Exception e) {
                    throw new ShenyuException("Could not load key store path ' " + keyStorePath + "'", e);
                }
                return keyStore;
            } catch (KeyStoreException | NoSuchProviderException e) {
                throw new ShenyuException("Could not load KeyStore for type and provider", e);
            }
        }
    }
}
