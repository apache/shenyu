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

package org.apache.shenyu.sync.data.api;

import org.apache.shenyu.common.dto.ProxyApiKeyData;

/**
 * AiProxyApiKeyDataSubscriber.
 */
public interface AiProxyApiKeyDataSubscriber {

    /**
     * On subscribe.
     *
     * @param data the proxy api key mapping data
     */
    void onSubscribe(ProxyApiKeyData data);

    /**
     * Un subscribe.
     *
     * @param data the proxy api key mapping data
     */
    void unSubscribe(ProxyApiKeyData data);

    /**
     * Refresh.
     */
    default void refresh() {
    }
} 