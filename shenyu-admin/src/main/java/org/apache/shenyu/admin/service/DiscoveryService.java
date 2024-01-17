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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;

import java.util.List;

public interface DiscoveryService {

    /**
     * the list of discovery type.
     *
     * @return the list of discovery type
     */
    List<String> typeEnums();

    /**
     * get the discovery by plugin and name.
     *
     * @param pluginName plugin name
     * @param level      level
     * @return the discovery
     */
    DiscoveryVO discovery(String pluginName, String level);

    /**
     * Create or update string.
     *
     * @param discoveryDTO the discovery dto
     * @return the string
     */
    DiscoveryVO createOrUpdate(DiscoveryDTO discoveryDTO);


    /**
     * registerDiscoveryConfig.
     * <p>
     * shenyu-client create discovery .
     * </p>
     *
     * @param discoveryConfigRegisterDTO discoveryConfigRegisterDTO
     */
    void registerDiscoveryConfig(DiscoveryConfigRegisterDTO discoveryConfigRegisterDTO);


    /**
     * delete by id.
     *
     * @param discoveryId discoveryId
     * @return msg
     */
    String delete(String discoveryId);

    /**
     * syncData.
     */
    void syncData();


    /**
     * findDiscoveryHandlerBySelectorId.
     *
     * @param selectorId selectorId
     * @return DiscoveryHandlerDTO
     */
    DiscoveryHandlerDTO findDiscoveryHandlerBySelectorId(String selectorId);


    /**
     * registerDefaultDiscovery.
     *
     * @param selectorId selectorId
     * @param pluginName pluginName
     * @return discoveryHandlerId
     */
    String registerDefaultDiscovery(String selectorId, String pluginName);
}
