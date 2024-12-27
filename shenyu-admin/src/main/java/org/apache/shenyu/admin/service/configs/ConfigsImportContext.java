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

import com.google.common.collect.Maps;

import java.util.Map;

public class ConfigsImportContext {

    /**
     * the import result.
     */
    private final Map<String, Object> result = Maps.newHashMap();

    /**
     * export selector id -> new or exist selector id.
     */
    private final Map<String, String> selectorIdMapping = Maps.newHashMap();

    /**
     * export proxy selector id -> new or exist proxy selector id.
     */
    private final Map<String, String> proxySelectorIdMapping = Maps.newHashMap();

    /**
     * export discovery handler id -> new or exist discovery handler id.
     */
    private final Map<String, String> discoveryHandlerIdMapping = Maps.newHashMap();

    /**
     * export plugin template id -> new or exist plugin template id.
     */
    private final Map<String, String> pluginTemplateIdMapping = Maps.newHashMap();

    public Map<String, Object> getResult() {
        return result;
    }

    public Map<String, String> getSelectorIdMapping() {
        return selectorIdMapping;
    }

    public Map<String, String> getProxySelectorIdMapping() {
        return proxySelectorIdMapping;
    }

    public Map<String, String> getDiscoveryHandlerIdMapping() {
        return discoveryHandlerIdMapping;
    }

    public Map<String, String> getPluginTemplateIdMapping() {
        return pluginTemplateIdMapping;
    }
}
