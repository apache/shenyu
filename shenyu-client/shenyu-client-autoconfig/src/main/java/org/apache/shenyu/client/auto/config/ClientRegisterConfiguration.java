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

package org.apache.shenyu.client.auto.config;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ContextApiRefreshedEventListener;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.apache.shenyu.client.core.register.matcher.Matcher;
import org.apache.shenyu.client.core.register.parser.ApiBeanMetaParser;
import org.apache.shenyu.client.core.register.parser.ApiDocParser;
import org.apache.shenyu.client.core.register.parser.ApiMetaParser;
import org.apache.shenyu.client.core.register.registrar.AbstractRegistrar;
import org.apache.shenyu.client.core.register.registrar.ApiBeanPreRegistrar;
import org.apache.shenyu.client.core.register.registrar.ApiBeanRegistrar;
import org.apache.shenyu.client.core.register.registrar.ApiRegistrar;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_DOC_BEAN_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_DOC_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_BEAN_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_BEAN_PRE_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_MATCHER;

@Configuration(proxyBeanMethods = false)
public class ClientRegisterConfiguration {

    /**
     * Gets ContextApiRefreshedEventListener Bean.
     *
     * @param apiBeanExtractor      apiBeanExtractor
     * @param apiMetaBeanRegistrars apiMetaBeanRegistrars
     * @param <T>                   ApiBean Type
     * @return contextApiRefreshedEventListener
     */
    @Bean
    public <T> ContextApiRefreshedEventListener<T> apiListener(final ApiBeansExtractor<T> apiBeanExtractor,
                                                               final List<AbstractRegistrar<ApiBean<T>>> apiMetaBeanRegistrars) {
        return new ContextApiRefreshedEventListener<>(apiMetaBeanRegistrars, apiBeanExtractor);
    }

    /**
     * Builds ApiMetaBeanRegistrar Bean.
     *
     * @param apiMetaBeanMatcher apiMetaBeanMatcher
     * @param apiMetaMatcher     apiMetaMatcher
     * @param apiMetaParser      apiMetaParser
     * @param publisher          publisher
     * @param <T>                ApiBean Type
     * @return apiBeanRegistrars
     */
    @Bean(name = "ApiMetaBeanRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.meta.enabled", matchIfMissing = true, havingValue = "true")
    public <T> ApiBeanRegistrar<T> buildApiMetaBeanRegistrar(@Qualifier(API_META_BEAN_MATCHER) final Matcher<ApiBean<T>> apiMetaBeanMatcher,
                                                             @Qualifier(API_META_MATCHER) final Matcher<ApiBean<T>.ApiDefinition> apiMetaMatcher,
                                                             final ApiMetaParser<T> apiMetaParser,
                                                             final ShenyuClientRegisterEventPublisher publisher) {

        ApiRegistrar<T, MetaDataRegisterDTO> apiMetaRegistrar =
                new ApiRegistrar<>(apiMetaMatcher, apiMetaParser, publisher);
        return new ApiBeanRegistrar<>(apiMetaBeanMatcher, apiMetaRegistrar);
    }

    /**
     * Builds ApiMetaBeanPreRegistrar Bean.
     *
     * @param apiMetaBeanPreMatcher apiMetaBeanPreMatcher
     * @param apiBeanMetaParser     apiBeanMetaParser
     * @param publisher             publisher
     * @param <T>                   ApiBean Type
     * @return apiBeanPreRegistrar
     */
    @Bean(name = "ApiMetaBeanPreRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.meta.enabled", matchIfMissing = true, havingValue = "true")
    public <T> ApiBeanPreRegistrar<T, DataTypeParent> buildApiMetaBeanPreRegistrar(final @Qualifier(API_META_BEAN_PRE_MATCHER) Matcher<ApiBean<T>> apiMetaBeanPreMatcher,
                                                                                   final ApiBeanMetaParser<T> apiBeanMetaParser,
                                                                                   final ShenyuClientRegisterEventPublisher publisher) {
        return new ApiBeanPreRegistrar<>(apiMetaBeanPreMatcher, apiBeanMetaParser, publisher);
    }

    /**
     * Builds ApiDocBeanRegistrar Bean.
     *
     * @param apiDocBeanMatcher apiDocBeanMatcher
     * @param apiDocMatcher     apiDocMatcher
     * @param apiDocParser      apiDocParser
     * @param publisher         publisher
     * @param <T>               ApiBean Type
     * @return apiBeanRegistrar
     */
    @Bean(name = "ApiDocBeanRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.doc.enabled", matchIfMissing = true, havingValue = "true")
    public <T> ApiBeanRegistrar<T> buildApiDocBeanRegistrar(@Qualifier(API_DOC_BEAN_MATCHER) final Matcher<ApiBean<T>> apiDocBeanMatcher,
                                                            @Qualifier(API_DOC_MATCHER) final Matcher<ApiBean<T>.ApiDefinition> apiDocMatcher,
                                                            final ApiDocParser<T> apiDocParser,
                                                            final ShenyuClientRegisterEventPublisher publisher) {

        ApiRegistrar<T, ApiDocRegisterDTO> apiDocRegistrar =
                new ApiRegistrar<>(apiDocMatcher, apiDocParser, publisher);
        return new ApiBeanRegistrar<>(apiDocBeanMatcher, apiDocRegistrar);
    }

    /**
     * Gets ShenyuClientRegisterEventPublisher Bean that is initialized .
     *
     * @param shenyuClientRegisterRepository shenyuClientRegisterRepository.
     * @return publisher
     */
    @Bean
    public ShenyuClientRegisterEventPublisher publisher(final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        return publisher;
    }
}
