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

package org.apache.shenyu.admin.service.configs;

public enum ConfigsExportImportEnum {

    /**
     * auth.
     */
    Auth("auth.json", 0),

    /**
     * meta.
     */
    Meta("meta.json", 1),

    /**
     * plugin.
     */
    Plugin("plugin.json", 2),

    /**
     * selector.
     */
    Selector("selector.json", 3),

    /**
     * rule.
     */
    Rule("rule.json", 4),

    /**
     * dict.
     */
    Dict("dict.json", 5),

    /**
     * proxy selector.
     */
    ProxySelector("proxy_selector.json", 6),

    /**
     * discovery.
     */
    Discovery("discovery.json", 7),

    /**
     * discovery upstream.
     */
    DiscoveryUpstream("discovery_upstream.json", 8);

    private String configName;

    /**
     * the import order.
     *
     * @return The smaller, the earlier it will be executed
     */
    private int importOrder;

    ConfigsExportImportEnum(final String configName, final int importOrder) {
        this.configName = configName;
        this.importOrder = importOrder;
    }

    public String getConfigName() {
        return configName;
    }

    public int getImportOrder() {
        return importOrder;
    }
}
