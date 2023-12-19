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
import org.apache.shenyu.admin.discovery.APDiscoveryProcessor;
import org.apache.shenyu.admin.discovery.LocalDiscoveryProcessor;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
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
     * @return DiscoveryProcessor
     */
    @Bean("DefaultDiscoveryProcessor")
    public DiscoveryProcessor discoveryDefaultProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        return new DefaultDiscoveryProcessor(discoveryUpstreamMapper);
    }

    /**
     * discoveryLocalProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     * @return LocalDiscoveryProcessor
     */
    @Bean("LocalDiscoveryProcessor")
    public DiscoveryProcessor discoveryLocalProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        return new LocalDiscoveryProcessor(discoveryUpstreamMapper);
    }

    /**
     * discoveryLocalProcessor.
     *
     * @param discoveryUpstreamMapper discoveryUpstreamMapper
     * @return DiscoveryProcessor
     */
    @Bean("APDiscoveryProcessor")
    public DiscoveryProcessor discoveryAPProcessor(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {
        return new APDiscoveryProcessor(discoveryUpstreamMapper);
    }

    /**
     * discoveryProcessorHolder.
     *
     * @param defaultDiscoveryProcessor defaultDiscoveryProcessor
     * @param localDiscoveryProcessor   localDiscoveryProcessor
     * @param eurekaDiscoveryProcessor  eurekaDiscoveryProcessor
     * @return DiscoveryProcessorHolder
     */
    @Bean
    public DiscoveryProcessorHolder discoveryProcessorHolder(@Qualifier("DefaultDiscoveryProcessor") final DiscoveryProcessor defaultDiscoveryProcessor,
                                                             @Qualifier("LocalDiscoveryProcessor") final DiscoveryProcessor localDiscoveryProcessor,
                                                             @Qualifier("APDiscoveryProcessor") final DiscoveryProcessor eurekaDiscoveryProcessor) {
        return new DiscoveryProcessorHolder(defaultDiscoveryProcessor, localDiscoveryProcessor, eurekaDiscoveryProcessor);
    }

}
