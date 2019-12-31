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

import com.google.common.collect.Sets;
import java.util.HashSet;
import java.util.Set;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.register.api.path.Path;

/**
 * RegisterDirectory
 * 1. Listen to the service interface for the relevant registration processing.
 *
 * @author sixh
 */
public abstract class RegisterDiscovery implements HealthCheck {

    private Set<RegisterDiscoveryListener> listeners = new HashSet<>();

    private String env;

    private URL url;

    private final String defEnv = "default";

    /**
     * Instantiates a new Register directory.
     *
     * @param url       the url.
     * @param listeners the listeners.
     */
    public RegisterDiscovery(URL url, Set<RegisterDiscoveryListener> listeners) {
        if (listeners != null) {
            this.listeners.addAll(listeners);
        }
        String env = url.getParameter(RegisterConst.EVN_KEY);
        if (StringUtils.isBlank(env)) {
            env = defEnv;
            url.putParameter(RegisterConst.EVN_KEY, env);
        }
        this.env = env;
        this.url = url;
    }

    /**
     * Instantiates a new Register directory.
     *
     * @param url      the url
     * @param listener the listener
     */
    public RegisterDiscovery(URL url, RegisterDiscoveryListener listener) {
        this(url, Sets.newHashSet(listener));
    }

    /**
     * Listener.
     *
     * @param listener the listener.
     */
    public void addListener(RegisterDiscoveryListener listener) {
        listeners.add(listener);
    }

    /**
     * Redress.
     *
     * @param paths the paths
     */
    protected void redress(Set<Path> paths) {
        listeners.forEach(listener -> listener.apply(paths));
    }

    /**
     * Environmental parameters.
     *
     * @return env env
     */
    public String getEnv() {
        return env;
    }

    /**
     * Registered address information.
     *
     * @return url url
     */
    public URL getUrl() {
        return url;
    }
}
