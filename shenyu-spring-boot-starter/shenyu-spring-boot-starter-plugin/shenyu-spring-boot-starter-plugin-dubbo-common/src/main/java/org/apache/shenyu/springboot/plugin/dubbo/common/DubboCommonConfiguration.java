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

package org.apache.shenyu.springboot.plugin.dubbo.common;

import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.dubbo.common.param.DubboParamResolveService;
import org.apache.shenyu.plugin.dubbo.common.context.DubboShenyuContextDecorator;
import org.apache.shenyu.plugin.dubbo.common.param.DubboParamResolveServiceImpl;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type Dubbo common configuration.
 */
@Configuration
public class DubboCommonConfiguration {
    
    /**
     * Dubbo shenyu context decorator shenyu context decorator.
     *
     * @return the shenyu context decorator
     */
    @Bean
    public ShenyuContextDecorator dubboShenyuContextDecorator() {
        return new DubboShenyuContextDecorator();
    }
    
    /**
     * Generic param resolve service dubbo param resolve service.
     *
     * @return the dubbo param resolve service
     */
    @Bean
    @ConditionalOnMissingBean(value = DubboParamResolveService.class, search = SearchStrategy.ALL)
    public DubboParamResolveService dubboParamResolveService() {
        return new DubboParamResolveServiceImpl();
    }
}
