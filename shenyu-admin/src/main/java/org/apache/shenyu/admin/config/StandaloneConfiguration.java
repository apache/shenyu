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

import org.apache.shenyu.admin.mode.ShenyuRunningModeService;
import org.apache.shenyu.admin.mode.standalone.ShenyuStandaloneService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Standalone configuration.
 */
@Configuration(proxyBeanMethods = false)
public class StandaloneConfiguration {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(StandaloneConfiguration.class);
    
    /**
     * Shenyu running mode standalone service.
     *
     * @param upstreamCheckService upstream check service
     * @param loadServiceDocEntry load service doc entry
     * @return Shenyu standalone service
     */
    @Bean(destroyMethod = "shutdown")
    @ConditionalOnProperty(value = {"shenyu.cluster.enabled"}, havingValue = "false", matchIfMissing = true)
    @ConditionalOnMissingBean
    public ShenyuRunningModeService shenyuRunningModeService(final UpstreamCheckService upstreamCheckService,
                                                             final LoadServiceDocEntry loadServiceDocEntry) {
        LOGGER.info("starting in standalone mode ...");
        return new ShenyuStandaloneService(
                upstreamCheckService,
                loadServiceDocEntry
        );
    }
    
}
