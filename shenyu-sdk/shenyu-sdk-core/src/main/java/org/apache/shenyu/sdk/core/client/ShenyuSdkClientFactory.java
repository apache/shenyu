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

package org.apache.shenyu.sdk.core.client;

import org.apache.shenyu.spi.ExtensionLoader;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type Shenyu sdk client factory.
 */
public class ShenyuSdkClientFactory {

    private static final Map<String, ShenyuSdkClient> SDK_CLIENT_MAP = new ConcurrentHashMap<>();
    
    /**
     * New shenyu sku client.
     *
     * @param clientType the client type
     * @return the shenyu instance register repository
     */
    public static ShenyuSdkClient newInstance(final String clientType) {
        if (!SDK_CLIENT_MAP.containsKey(clientType)) {
            ShenyuSdkClient result = ExtensionLoader.getExtensionLoader(ShenyuSdkClient.class).getJoin(clientType);
            SDK_CLIENT_MAP.put(clientType, result);
            return result;
        }
        return SDK_CLIENT_MAP.get(clientType);
    }
}
