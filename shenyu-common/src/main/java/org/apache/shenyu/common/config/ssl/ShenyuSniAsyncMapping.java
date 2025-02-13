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

package org.apache.shenyu.common.config.ssl;

import io.netty.util.AsyncMapping;
import io.netty.util.concurrent.Future;
import io.netty.util.concurrent.Promise;
import org.apache.shenyu.common.exception.ShenyuException;
import reactor.netty.http.Http11SslContextSpec;
import reactor.netty.tcp.SslProvider;
import reactor.netty.tcp.TcpSslContextSpec;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

/**
 * Sni async map, can be used to dynamically configure ssl certificates.
 */
public class ShenyuSniAsyncMapping implements AsyncMapping<String, SslProvider> {

    private final ConcurrentHashMap<String, SslProvider> sslProviderMap;

    public ShenyuSniAsyncMapping() {
        this.sslProviderMap = new ConcurrentHashMap<>();
    }

    public ShenyuSniAsyncMapping(final List<SslCrtAndKeyFile> sslCrtAndKeys) {
        if (Objects.isNull(sslCrtAndKeys) || sslCrtAndKeys.isEmpty()) {
            throw new ShenyuException("The sslCrtAndKeys can not be null");
        }
        this.sslProviderMap = new ConcurrentHashMap<>();
        sslCrtAndKeys.forEach(sslCrtAndKey -> {
            Http11SslContextSpec sslContextSpec = Http11SslContextSpec.forServer(new File(sslCrtAndKey.getKeyCertChainFile()),
                    new File(sslCrtAndKey.getKeyFile()));
            SslProvider sslProvider = SslProvider.builder().sslContext(sslContextSpec).build();
            this.sslProviderMap.put(sslCrtAndKey.getDomain(), sslProvider);
        });
    }

    /**
     * Add SslProvider by domain.
     *
     * @param domain domain
     * @param sslProvider SslProvider
     */
    public void addSslProvider(final String domain, final SslProvider sslProvider) {
        sslProviderMap.put(domain, sslProvider);
    }

    /**
     * Add ssl config.
     *
     * @param sslCrtAndKey sslCrtAndKey
     * @throws IOException IOException
     */
    public void addSslCertificate(final SslCrtAndKey sslCrtAndKey) throws IOException {
        if (sslCrtAndKey instanceof SslCrtAndKeyFile) {
            SslCrtAndKeyFile sslCrtAndKeyFile = (SslCrtAndKeyFile) sslCrtAndKey;
            TcpSslContextSpec sslContextSpec = TcpSslContextSpec.forServer(new File(sslCrtAndKeyFile.getKeyCertChainFile()),
                    new File(sslCrtAndKeyFile.getKeyFile()));
            SslProvider sslProvider = SslProvider.builder().sslContext(sslContextSpec).build();
            this.sslProviderMap.put(sslCrtAndKeyFile.getDomain(), sslProvider);
        } else if (sslCrtAndKey instanceof SslCrtAndKeyStream) {
            SslCrtAndKeyStream sslCrtAndKeyStream = (SslCrtAndKeyStream) sslCrtAndKey;
            sslCrtAndKeyStream.getKeyCertChainInputStream().reset();
            sslCrtAndKeyStream.getKeyInputStream().reset();
            TcpSslContextSpec sslContextSpec = TcpSslContextSpec.forServer(sslCrtAndKeyStream.getKeyCertChainInputStream(),
                    sslCrtAndKeyStream.getKeyInputStream());
            SslProvider sslProvider = SslProvider.builder().sslContext(sslContextSpec).build();
            this.sslProviderMap.put(sslCrtAndKeyStream.getDomain(), sslProvider);
        }
    }

    /**
     * Remove ssl config by domain.
     *
     * @param domain domain
     */
    public void removeSslCertificate(final String domain) {
        this.sslProviderMap.remove(domain);
    }

    /**
     * Get SslProvider by domain.
     *
     * @param domain domain
     * @param promise the promise of SslProvider
     * @return SslProvider Future
     */
    @Override
    public Future<SslProvider> map(final String domain, final Promise<SslProvider> promise) {
        try {
            for (String key : sslProviderMap.keySet()) {
                if (matchDomain(domain, key)) {
                    return promise.setSuccess(sslProviderMap.get(key));
                }
            }
            return promise.setFailure(new ShenyuException(
                    String.format("Can not find ssl certificate of domain %s", domain)));
        } catch (Throwable cause) {
            return promise.setFailure(cause);
        }
    }

    private boolean matchDomain(final String domain, final String pattern) {
        return Pattern.matches(pattern.replace(".", "\\.").replace("*", ".*"), domain);
    }
}
