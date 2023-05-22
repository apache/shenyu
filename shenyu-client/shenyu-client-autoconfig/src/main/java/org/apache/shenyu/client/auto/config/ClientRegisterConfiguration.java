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
import org.apache.shenyu.client.core.register.ClientApiRefreshedEventListener;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.apache.shenyu.client.core.register.matcher.Matcher;
import org.apache.shenyu.client.core.register.parser.ApiDocDefinitionParser;
import org.apache.shenyu.client.core.register.parser.ApiMetaDefinitionParser;
import org.apache.shenyu.client.core.register.parser.PreApiMetaBeanParser;
import org.apache.shenyu.client.core.register.registrar.AbstractRegistrar;
import org.apache.shenyu.client.core.register.registrar.ApiBeanRegistrar;
import org.apache.shenyu.client.core.register.registrar.ApiRegistrar;
import org.apache.shenyu.client.core.register.registrar.PreApiBeanRegistrar;
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
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_DOC_DEFINITION_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_BEAN_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_DEFINITION_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.PRE_API_META_BEAN_MATCHER;

@Configuration(proxyBeanMethods = false)
public class ClientRegisterConfiguration {

    /**
     * Gets ContextApiRefreshedEventListener Bean.
     *
     * @param apiBeanExtractor      apiBeanExtractor
     * @param apiMetaBeanRegistrars apiMetaBeanRegistrars
     * @return contextApiRefreshedEventListener
     */
    @Bean
    public ClientApiRefreshedEventListener apiListener(final ApiBeansExtractor apiBeanExtractor,
                                                       final List<AbstractRegistrar<ApiBean>> apiMetaBeanRegistrars) {
        return new ClientApiRefreshedEventListener(apiMetaBeanRegistrars, apiBeanExtractor);
    }

    /**
     * Builds ApiMetaBeanRegistrar Bean.
     *
     * @param apiMetaBeanMatcher      apiMetaBeanMatcher
     * @param apiMetaMatcher          apiMetaMatcher
     * @param apiMetaDefinitionParser apiMetaParser
     * @param publisher               publisher
     * @return apiBeanRegistrars
     */
    @Bean(name = "ApiMetaBeanRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.meta.enabled", matchIfMissing = true, havingValue = "true")
    public ApiBeanRegistrar buildApiMetaBeanRegistrar(@Qualifier(API_META_BEAN_MATCHER) final Matcher<ApiBean> apiMetaBeanMatcher,
                                                      @Qualifier(API_META_DEFINITION_MATCHER) final Matcher<ApiBean.ApiDefinition> apiMetaMatcher,
                                                      final ApiMetaDefinitionParser apiMetaDefinitionParser,
                                                      final ShenyuClientRegisterEventPublisher publisher) {

        ApiRegistrar<MetaDataRegisterDTO> apiMetaRegistrar =
                new ApiRegistrar<>(apiMetaMatcher, apiMetaDefinitionParser, publisher);
        return new ApiBeanRegistrar(apiMetaBeanMatcher, apiMetaRegistrar);
    }

    /**
     * Builds ApiMetaBeanPreRegistrar Bean.
     *
     * @param preApiMetaBeanMatcher apiMetaBeanPreMatcher
     * @param preApiBeanMetaParser  preApiBeanMetaParser
     * @param publisher             publisher
     * @return apiBeanPreRegistrar
     */
    @Bean(name = "PreApiMetaBeanRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.meta.enabled", matchIfMissing = true, havingValue = "true")
    public PreApiBeanRegistrar<DataTypeParent> buildApiMetaBeanPreRegistrar(final @Qualifier(PRE_API_META_BEAN_MATCHER) Matcher<ApiBean> preApiMetaBeanMatcher,
                                                                            final PreApiMetaBeanParser preApiBeanMetaParser,
                                                                            final ShenyuClientRegisterEventPublisher publisher) {
        return new PreApiBeanRegistrar<>(preApiMetaBeanMatcher, preApiBeanMetaParser, publisher);
    }

    /**
     * Builds ApiDocBeanRegistrar Bean.
     *
     * @param apiDocBeanMatcher      apiDocBeanMatcher
     * @param apiDocMatcher          apiDocMatcher
     * @param apiDocDefinitionParser apiDocParser
     * @param publisher              publisher
     * @return apiBeanRegistrar
     */
    @Bean(name = "ApiDocBeanRegistrar")
    @ConditionalOnProperty(value = "shenyu.register.api.doc.enabled", matchIfMissing = true, havingValue = "true")
    public ApiBeanRegistrar buildApiDocBeanRegistrar(@Qualifier(API_DOC_BEAN_MATCHER) final Matcher<ApiBean> apiDocBeanMatcher,
                                                     @Qualifier(API_DOC_DEFINITION_MATCHER) final Matcher<ApiBean.ApiDefinition> apiDocMatcher,
                                                     final ApiDocDefinitionParser apiDocDefinitionParser,
                                                     final ShenyuClientRegisterEventPublisher publisher) {

        ApiRegistrar<ApiDocRegisterDTO> apiDocRegistrar =
                new ApiRegistrar<>(apiDocMatcher, apiDocDefinitionParser, publisher);
        return new ApiBeanRegistrar(apiDocBeanMatcher, apiDocRegistrar);
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
