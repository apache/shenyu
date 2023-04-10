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

import org.apache.shenyu.admin.listener.AbstractListDataChangedListener;
import org.apache.shenyu.common.constant.ApolloPathConstants;
import org.springframework.util.StringUtils;

/**
 * use apollo to push data changes.
 *
 * @since 2.6.0
 */
public class ApolloDataChangedListener extends AbstractListDataChangedListener {
    private final ApolloClient apolloClient;

    /**
     * Instantiates a new apollo data changed listener.
     *
     * @param apolloClient the apollo client
     */
    public ApolloDataChangedListener(final ApolloClient apolloClient) {
        super(new ChangeData(ApolloPathConstants.PLUGIN_DATA_ID, ApolloPathConstants.SELECTOR_DATA_ID,
                ApolloPathConstants.RULE_DATA_ID, ApolloPathConstants.AUTH_DATA_ID, ApolloPathConstants.META_DATA_ID));
        this.apolloClient = apolloClient;
    }

    @Override
    public void publishConfig(final String dataId, final Object data) {
        this.apolloClient.createOrUpdateItem(dataId, data, "create config data");
        this.apolloClient.publishNamespace("publish config data", "");
    }

    @Override
    public String getConfig(final String dataId) {
        String config = this.apolloClient.getItemValue(dataId);
        return StringUtils.hasLength(config) ? config : ApolloPathConstants.EMPTY_CONFIG_DEFAULT_VALUE;
    }
}
