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

package org.apache.shenyu.register.common.config;

import org.apache.shenyu.common.exception.ShenyuException;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * The type Shenyu client config.
 */
public final class ShenyuClientConfig {
    
    private Map<String, ClientPropertiesConfig> client = new HashMap<String, ClientPropertiesConfig>() {
        @Override
        public ClientPropertiesConfig get(final Object key) {
            ClientPropertiesConfig config = super.get(key);
            if (Objects.isNull(key) || Objects.isNull(config)) {
                throw new ShenyuException("key is null or invalid, you should checkout property of " + key);
            }
            return config;
        }
    };
    
    /**
     * Gets client.
     *
     * @return the client
     */
    public Map<String, ClientPropertiesConfig> getClient() {
        return client;
    }
    
    /**
     * Sets client.
     *
     * @param client the client
     */
    public void setClient(final Map<String, ClientPropertiesConfig> client) {
        this.client = client;
    }

    /**
     * this client properties config.
     */
    public static class ClientPropertiesConfig extends PropertiesConfig {

    }
}
