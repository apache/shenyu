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

package org.apache.shenyu.spring.boot.starter.plugin.global;

import org.apache.shenyu.plugin.api.SoulPlugin;
import org.apache.shenyu.plugin.api.context.SoulContextBuilder;
import org.apache.shenyu.plugin.api.context.SoulContextDecorator;
import org.apache.shenyu.plugin.global.DefaultSoulContextBuilder;
import org.apache.shenyu.plugin.global.GlobalPlugin;
import org.apache.shenyu.plugin.global.subsciber.MetaDataAllSubscriber;
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
 * The type Global plugin configuration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
@ConditionalOnClass(GlobalPlugin.class)
public class GlobalPluginConfiguration {
    
    /**
     * Global plugin soul plugin.
     *
     * @param soulContextBuilder the soul context builder
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin globalPlugin(final SoulContextBuilder soulContextBuilder) {
        return new GlobalPlugin(soulContextBuilder);
    }
    
    /**
     * Soul context builder soul context builder.
     *
     * @param decorators the decorators
     * @return the soul context builder
     */
    @Bean
    @ConditionalOnMissingBean(value = SoulContextBuilder.class, search = SearchStrategy.ALL)
    public SoulContextBuilder soulContextBuilder(final ObjectProvider<List<SoulContextDecorator>> decorators) {
        List<SoulContextDecorator> decoratorList = decorators.getIfAvailable(Collections::emptyList);
        Map<String, SoulContextDecorator> decoratorMap = decoratorList.stream().collect(Collectors.toMap(SoulContextDecorator::rpcType, e -> e));
        return new DefaultSoulContextBuilder(decoratorMap);
    }
    
    /**
     * Data subscriber meta data subscriber.
     *
     * @return the meta data subscriber
     */
    @Bean
    public MetaDataSubscriber metaDataAllSubscriber() {
        return new MetaDataAllSubscriber();
    }
}
