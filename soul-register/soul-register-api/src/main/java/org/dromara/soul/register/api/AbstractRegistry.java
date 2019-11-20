/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.api;

import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.http.URL;

/**
 * AbstractRegistry.
 * soul Implementation of service registration.
 *
 * @author sixh
 */
public abstract class AbstractRegistry implements Registry {

    private URL remoteUrl;

    private String protocol;

    /**
     * Instantiates a new Abstract registry.
     *
     * @param url the url
     */
    public AbstractRegistry(URL url) {
        setRemoteUrl(url);
    }

    private void setRemoteUrl(URL url) {
        if (url == null) {
            throw new SoulException("remote url is null");
        }
        this.remoteUrl = url;
        this.protocol = url.getProtocol();
        if (!this.protocol.equals(urlProtocol())) {
            throw new SoulException(" url protocol is error");
        }
    }

    public void retry() {
        //todo:重试.
    }

    /**
     * Gets protocol.
     *
     * @return the protocol
     */
    public String getProtocol() {
        return protocol;
    }

    /**
     * Gets remote url.
     *
     * @return the remote url
     */
    public URL getRemoteUrl() {
        return remoteUrl;
    }

    /**
     * remoteUrl Url protocol string.
     *
     * @return the string
     * @see org.dromara.soul.register.api.config.RegistryConfig
     */
    public abstract String urlProtocol();
}
