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

package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;

import java.util.List;

/**
 * shenyu client register service factory.
 */
public interface ShenyuClientRegisterService {
    
    /**
     * Rpc type string.
     *
     * @return the string
     */
    String rpcType();
    
    /**
     * Register meta data.
     *
     * @param metaDataRegisterDTO meta data register dto.
     * @return the string
     */
    String register(MetaDataRegisterDTO metaDataRegisterDTO);

    /**
     * registerApiDoc.
     * @param apiDocRegisterDTO apiDocRegisterDTO
     * @return String
     */
    String registerApiDoc(ApiDocRegisterDTO apiDocRegisterDTO);

    /**
     * check whether the namespace and plugin relation exists.
     *
     * @param namespaceId namespaceId
     * @param pluginName plugin name
     */
    void checkNamespacePluginRel(String namespaceId, String pluginName);
    
    /**
     * Register uri string.
     *
     * @param selectorName the selector name
     * @param uriList the uri list
     * @param namespaceId the namespace id
     * @return the string
     */
    default String registerURI(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return Constants.SUCCESS;
    }
    
    /**
     * Register heartbeat.
     *
     * @param selectorName the selector name
     * @param uriList the uri list
     * @param namespaceId the namespace id
     * @return the string
     */
    default String heartbeat(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return Constants.SUCCESS;
    }
    
    /**
     * offline.
     *
     * @param selectorName the selector name
     * @param uriList the uri list
     * @param namespaceId the namespace id
     * @return the string
     */
    default String offline(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return Constants.SUCCESS;
    }
    
    /**
     * Register context path.
     *
     * @param metaDataRegisterDTO the meta data register dto
     */
    default void registerContextPath(final MetaDataRegisterDTO metaDataRegisterDTO) {
    }
}
