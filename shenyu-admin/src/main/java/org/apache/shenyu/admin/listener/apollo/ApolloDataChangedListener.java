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

package org.apache.shenyu.admin.listener.apollo;

import org.apache.shenyu.admin.listener.AbstractNodeDataChangedListener;
import org.apache.shenyu.register.client.server.apollo.ApolloClient;

/**
 * use apollo to push data changes.
 *
 * @since 2.6.0
 */
public class ApolloDataChangedListener extends AbstractNodeDataChangedListener {
    private final ApolloClient apolloClient;

    /**
     * Instantiates a new apollo data changed listener.
     *
     * @param apolloClient the apollo client
     */
    public ApolloDataChangedListener(final ApolloClient apolloClient) {
        this.apolloClient = apolloClient;
    }

    @Override
    public void createOrUpdate(final String pluginPath, final Object data) {
        this.apolloClient.createOrUpdateItem(pluginPath, data, "");
        this.apolloClient.publishNamespace("create or update node data", "");
    }

    @Override
    public void deleteNode(final String pluginPath) {
        this.apolloClient.removeItem(pluginPath);
    }

    @Override
    public void deletePathRecursive(final String selectorParentPath) {
        this.apolloClient.removeItem(selectorParentPath);
    }
}
