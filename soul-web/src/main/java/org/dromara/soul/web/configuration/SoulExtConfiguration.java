/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.configuration;

import org.dromara.soul.web.filter.DefaultParamService;
import org.dromara.soul.web.filter.ParamService;
import org.dromara.soul.web.plugin.before.DefaultSignService;
import org.dromara.soul.web.plugin.before.SignService;
import org.dromara.soul.web.result.SoulDefaultResult;
import org.dromara.soul.web.result.SoulResult;
import org.dromara.soul.web.support.RemoteAddressResolver;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;

/**
 * The type Soul result configuration.
 *
 * @author xiaoyu
 */
public class SoulExtConfiguration {

    /**
     * Soul result soul result.
     *
     * @return the soul result
     */
    @Bean
    @ConditionalOnMissingBean(value = SoulResult.class, search = SearchStrategy.ALL)
    public SoulResult soulResult() {
        return new SoulDefaultResult();
    }


    /**
     * Remote address resolver remote address resolver.
     *
     * @return the remote address resolver
     */
    @Bean
    @ConditionalOnMissingBean(value = RemoteAddressResolver.class, search = SearchStrategy.ALL)
    public RemoteAddressResolver remoteAddressResolver() {
        return new RemoteAddressResolver() {
        };
    }

    /**
     * Param service param service.
     *
     * @return the param service
     */
    @Bean
    @ConditionalOnMissingBean(ParamService.class)
    public ParamService paramService() {
        return new DefaultParamService();
    }

}
