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

package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;

import java.util.List;

/**
 * DiscoveryProcessor.
 */
public interface DiscoveryProcessor {

    /**
     * createDiscovery.
     *
     * @param discoveryDO discoveryDO
     */
    void createDiscovery(DiscoveryDO discoveryDO);

    /**
     * createProxySelector.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @param proxySelectorDTO    proxySelectorDTO
     */
    void createProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO);

    /**
     * removeDiscovery.
     *
     * @param discoveryDO discoveryDO
     */
    void removeDiscovery(DiscoveryDO discoveryDO);

    /**
     * removeProxySelector.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @param proxySelectorDTO    proxySelectorDTO
     */
    void removeProxySelector(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO);

    /**
     * only use in local mode to sync upstreamDTOS.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @param upstreamDTOS     upstreamDTOS
     */
    void changeUpstream(ProxySelectorDTO proxySelectorDTO, List<DiscoveryUpstreamDTO> upstreamDTOS);


    /**
     * sync all upstream list to gateway and db.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @param proxySelectorDTO    proxySelectorDTO
     */
    void fetchAll(DiscoveryHandlerDTO discoveryHandlerDTO, ProxySelectorDTO proxySelectorDTO);

    /**
     * remove selector upstream.
     *
     * @param proxySelectorDTO    proxySelectorDTO
     */
    void removeSelectorUpstream(ProxySelectorDTO proxySelectorDTO);

}
