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

package org.apache.shenyu.registry.kubernetes;

import java.util.ArrayList;
import java.util.List;

public class KubernetesConfig {

    private String discoveryServerUrl;

    private boolean enabled = true;

    private List<String> namespaces = new ArrayList();

    public KubernetesConfig() {
    }

    /**
     * get discoveryServer url.
     * @return discoveryServer url.
     */
    public String getDiscoveryServerUrl() {
        return this.discoveryServerUrl;
    }

    /**
     * set discoveryServer url.
     * @param discoveryServerUrl discoveryServer url.
     */
    public void setDiscoveryServerUrl(final String discoveryServerUrl) {
        this.discoveryServerUrl = discoveryServerUrl;
    }

    /**
     * get enable status.
     * @return enable status.
     */
    public boolean isEnabled() {
        return this.enabled;
    }

    /**
     * set registry enable status.
     * @param enabled enable status.
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * get namespaces.
     * @return namespaces.
     */
    List<String> getNamespaces() {
        return this.namespaces;
    }

    /**
     * set kubernetes namespace.
     * @param namespaces list of namespace.
     */
    public void setNamespaces(final List<String> namespaces) {
        this.namespaces = namespaces;
    }
}
