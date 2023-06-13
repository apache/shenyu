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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.discovery.DefaultDiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * DiscoveryConfiguration.
 */
@Configuration
public class DiscoveryConfiguration {

    /**
     * discoveryProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     * @param proxySelectorMapper     proxySelectorMapper
     * @return DiscoveryProcessor
     */
    @Bean("DefaultDiscoveryProcessor")
    public DiscoveryProcessor discoveryDefaultProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper, final ProxySelectorMapper proxySelectorMapper) {
        return new DefaultDiscoveryProcessor(discoveryUpstreamMapper, proxySelectorMapper);
    }

    /**
     * discoveryLocalProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     * @param proxySelectorMapper     proxySelectorMapper
     * @return LocalDiscoveryProcessor
     */
    @Bean("LocalDiscoveryProcessor")
    public DiscoveryProcessor discoveryLocalProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper, final ProxySelectorMapper proxySelectorMapper) {
        return new DefaultDiscoveryProcessor(discoveryUpstreamMapper, proxySelectorMapper);
    }

    /**
     * discoveryProcessorHolder.
     *
     * @param defaultDiscoveryProcessor defaultDiscoveryProcessor
     * @param localDiscoveryProcessor   localDiscoveryProcessor
     * @return DiscoveryProcessorHolder
     */
    @Bean
    public DiscoveryProcessorHolder discoveryProcessorHolder(@Qualifier("DefaultDiscoveryProcessor") final DiscoveryProcessor defaultDiscoveryProcessor,
                                                             @Qualifier("LocalDiscoveryProcessor") final DiscoveryProcessor localDiscoveryProcessor
    ) {
        DiscoveryProcessorHolder discoveryProcessorHolder = new DiscoveryProcessorHolder(defaultDiscoveryProcessor, localDiscoveryProcessor);
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setType("zookeeper");
        discoveryDO.setServiceList("127.0.0.1:2181");
        discoveryDO.setProps("{}");
        discoveryDO.setId("1");
        defaultDiscoveryProcessor.createDiscovery(discoveryDO);
        DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
        discoveryHandlerDTO.setDiscoveryId("1");
        discoveryHandlerDTO.setProps("{}");
        ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
        proxySelectorDTO.setId("1");
        proxySelectorDTO.setName("TcpProxySelector");
        proxySelectorDTO.setForwardPort(9600);
        proxySelectorDTO.setPluginName(PluginEnum.TCP.getName());
        defaultDiscoveryProcessor.createProxySelector(discoveryHandlerDTO, proxySelectorDTO);
        return discoveryProcessorHolder;
    }

}
