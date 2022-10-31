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

package org.apache.shenyu.springboot.starter.plugin.global;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextBuilder;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.global.DefaultShenyuContextBuilder;
import org.apache.shenyu.plugin.global.GlobalPlugin;
import org.apache.shenyu.plugin.global.subsciber.MetaDataCacheSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.SearchStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The type global plugin configuration.
 */
@Configuration
@ConditionalOnClass(GlobalPlugin.class)
public class GlobalPluginConfiguration {
    
    /**
     * Global plugin shenyu plugin.
     *
     * @param shenyuContextBuilder the shenyu context builder
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin globalPlugin(final ShenyuContextBuilder shenyuContextBuilder) {
        return new GlobalPlugin(shenyuContextBuilder);
    }
    
    /**
     * Shenyu context builder.
     *
     * @param decorators the decorators
     * @return the shenyu context builder
     */
    @Bean
    @ConditionalOnMissingBean(value = ShenyuContextBuilder.class, search = SearchStrategy.ALL)
    public ShenyuContextBuilder shenyuContextBuilder(final ObjectProvider<List<ShenyuContextDecorator>> decorators) {
        List<ShenyuContextDecorator> decoratorList = decorators.getIfAvailable(Collections::emptyList);
        Map<String, ShenyuContextDecorator> decoratorMap = decoratorList.stream().collect(Collectors.toMap(ShenyuContextDecorator::rpcType, e -> e));
        return new DefaultShenyuContextBuilder(decoratorMap);
    }
    
    /**
     * Cache meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber metaDataCacheSubscriber() {
        return new MetaDataCacheSubscriber();
    }
}
