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

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.collect.Sets;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.register.api.exception.RegisterException;

/**
 * AbstractRegistry.
 * soul Implementation of service registration.
 *
 * @author sixh
 */
public abstract class AbstractRegistry implements Registry {

    /**
     * Register the server address.
     */
    private URL remoteUrl;

    private String protocol;

    /**
     * The notifyUrls.
     */
    private final Map<URL, RegisterNotifyListener> subscribed = new ConcurrentHashMap<>();

    /**
     * The Registered.
     */
    private final Set<URL> registered = Sets.newConcurrentHashSet();

    /**
     * Instantiates a new Abstract registry.
     *
     * @param url the url.
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

    /**
     * get subscribed.
     *
     * @return map.
     */
    Map<URL, RegisterNotifyListener> getSubscribed() {
        return Collections.unmodifiableMap(subscribed);
    }

    /**
     * get registered.
     *
     * @return set.
     */
    Set<URL> getRegistered() {
        return Collections.unmodifiableSet(registered);
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
    protected URL getRemoteUrl() {
        return remoteUrl;
    }

    /**
     * remoteUrl Url protocol string.
     *
     * @return the string
     * @see org.dromara.soul.register.api.config.RegistryConfig
     */
    public abstract String urlProtocol();

    @Override
    public void register(URL url) {
        if (url == null) {
            throw new RegisterException("url is null || url == null");
        }
        registered.add(url);
    }


    @Override
    public void unregister(URL url) {
        if (url == null) {
            throw new RegisterException("url is null || url == null");
        }
        registered.remove(url);
    }

    @Override
    public void subscribe(URL url, RegisterNotifyListener listener) {
        if (url == null) {
            throw new RegisterException("url is null || url == null");
        }
        if (listener == null) {
            throw new RegisterException("listener is null || listener == null");
        }
        subscribed.putIfAbsent(url, listener);
    }

    @Override
    public void unsubscribe(URL url) {
        if (url == null) {
            throw new RegisterException("url is null || url == null");
        }
        subscribed.remove(url);
    }

    @Override
    public List<URL> pull(URL url) {
        return null;
    }
}
