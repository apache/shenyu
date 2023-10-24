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

package org.apache.shenyu.k8s.common;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;

import java.util.List;

/**
 * The configuration that shenyu can read directly.
 */
public class ShenyuMemoryConfig {

    private Pair<Pair<String, String>, IngressConfiguration> globalDefaultBackend;

    private List<IngressConfiguration> routeConfigList;

    private List<SslCrtAndKeyStream> tlsConfigList;

    /**
     * ShenyuMemoryConfig Constructor.
     */
    public ShenyuMemoryConfig() {
    }

    /**
     * Get GlobalDefaultBackend.
     *
     * @return GlobalDefaultBackend
     */
    public Pair<Pair<String, String>, IngressConfiguration> getGlobalDefaultBackend() {
        return globalDefaultBackend;
    }

    /**
     * Set GlobalDefaultBackend.
     *
     * @param globalDefaultBackend GlobalDefaultBackend
     */
    public void setGlobalDefaultBackend(final Pair<Pair<String, String>, IngressConfiguration> globalDefaultBackend) {
        this.globalDefaultBackend = globalDefaultBackend;
    }

    /**
     * Get RouteConfigList.
     *
     * @return RouteConfigList
     */
    public List<IngressConfiguration> getRouteConfigList() {
        return routeConfigList;
    }

    /**
     * Set RouteConfigList.
     *
     * @param routeConfigList RouteConfigList
     */
    public void setRouteConfigList(final List<IngressConfiguration> routeConfigList) {
        this.routeConfigList = routeConfigList;
    }

    /**
     * Get TlsConfigList.
     *
     * @return TlsConfigList
     */
    public List<SslCrtAndKeyStream> getTlsConfigList() {
        return tlsConfigList;
    }

    /**
     * Set TlsConfigList.
     *
     * @param tlsConfigList TlsConfigList
     */
    public void setTlsConfigList(final List<SslCrtAndKeyStream> tlsConfigList) {
        this.tlsConfigList = tlsConfigList;
    }
}
