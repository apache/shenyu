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

package org.apache.shenyu.register.client.server.apollo;

import com.ctrip.framework.apollo.openapi.client.ApolloOpenApiClient;
import com.ctrip.framework.apollo.openapi.dto.NamespaceReleaseDTO;
import com.ctrip.framework.apollo.openapi.dto.OpenItemDTO;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Date;

/**
 * apollo open-api client.
 */
public class ApolloClient {

    private static final String DEFAULT_USER = "apollo";

    private final ApolloConfig apolloConfig;

    private final ApolloOpenApiClient apolloOpenApiClient;

    public ApolloClient(final ApolloConfig apolloConfig) {
        this.apolloConfig = apolloConfig;

        this.apolloOpenApiClient = ApolloOpenApiClient
                .newBuilder()
                .withPortalUrl(apolloConfig.getPortalUrl())
                .withToken(apolloConfig.getToken())
                .build();
    }

    /**
     * get item value.
     * @param key item key
     * @return item value
     */
    public String getItemValue(final String key) {
        OpenItemDTO openItemDTO = this.apolloOpenApiClient.getItem(
                apolloConfig.getAppId(),
                apolloConfig.getEnv(),
                apolloConfig.getClusterName(),
                apolloConfig.getNamespace(),
                key
        );
        // no such key
        if (openItemDTO == null) {
            return null;
        }
        // todo handle timeout exception
        if ("timeout".equals(openItemDTO.getKey())) {
            return null;
        }

        return openItemDTO.getValue();
    }

    /**
     * create or update item into namespace.
     * @param key item key
     * @param value item value
     * @param comment item comment
     */
    public void createOrUpdateItem(final String key, final Object value, final String comment) {
        this.createOrUpdateItem(key, GsonUtils.getInstance().toJson(value), comment);
    }

    /**
     * create or update item into namespace.
     * @param key item key
     * @param value item value
     * @param comment item comment
     */
    public void createOrUpdateItem(final String key, final String value, final String comment) {
        OpenItemDTO openItemDTO = new OpenItemDTO();
        openItemDTO.setKey(key);
        openItemDTO.setValue(value);
        openItemDTO.setComment(comment);
        openItemDTO.setDataChangeCreatedBy(DEFAULT_USER);
        openItemDTO.setDataChangeLastModifiedBy(DEFAULT_USER);
        Date now = new Date();
        openItemDTO.setDataChangeCreatedTime(now);
        openItemDTO.setDataChangeLastModifiedTime(now);

        this.apolloOpenApiClient.createOrUpdateItem(
                apolloConfig.getAppId(),
                apolloConfig.getEnv(),
                apolloConfig.getClusterName(),
                apolloConfig.getNamespace(),
                openItemDTO
        );
    }

    /**
     * remove item from namespace.
     * @param key item key
     */
    public void removeItem(final String key) {
        this.apolloOpenApiClient.removeItem(
                apolloConfig.getAppId(),
                apolloConfig.getEnv(),
                apolloConfig.getClusterName(),
                apolloConfig.getNamespace(),
                key,
                DEFAULT_USER
        );
    }

    /**
     * publish item list in namespace.
     * @param releaseTitle publish release title
     * @param releaseComment publish release comment
     */
    public void publishNamespace(final String releaseTitle, final String releaseComment) {
        NamespaceReleaseDTO namespaceReleaseDTO = new NamespaceReleaseDTO();
        namespaceReleaseDTO.setReleaseTitle(releaseTitle);
        namespaceReleaseDTO.setReleaseComment(releaseComment);
        namespaceReleaseDTO.setReleasedBy(DEFAULT_USER);

        this.apolloOpenApiClient.publishNamespace(
                apolloConfig.getAppId(),
                apolloConfig.getEnv(),
                apolloConfig.getClusterName(),
                apolloConfig.getNamespace(),
                namespaceReleaseDTO
        );
    }
}
