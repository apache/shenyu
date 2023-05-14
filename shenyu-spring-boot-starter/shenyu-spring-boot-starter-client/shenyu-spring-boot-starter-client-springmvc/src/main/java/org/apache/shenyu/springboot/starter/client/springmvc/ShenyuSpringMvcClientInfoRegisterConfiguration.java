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

package org.apache.shenyu.springboot.starter.client.springmvc;

import org.apache.shenyu.client.auto.config.ClientRegisterConfiguration;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientInfoRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.apache.shenyu.client.core.register.matcher.Matcher;
import org.apache.shenyu.client.core.register.matcher.apidoc.DefaultApiDocBeanMatcher;
import org.apache.shenyu.client.core.register.matcher.apidoc.DefaultApiDocDefinitionMatcher;
import org.apache.shenyu.client.core.register.matcher.apimeta.DefaultApiMetaBeanMatcher;
import org.apache.shenyu.client.core.register.matcher.apimeta.DefaultApiMetaDefinitionMatcher;
import org.apache.shenyu.client.core.register.matcher.apimeta.DefaultPreApiMetaBeanMatcher;
import org.apache.shenyu.client.core.register.parser.apidoc.ApiDocDefinitionParser;
import org.apache.shenyu.client.core.register.parser.apidoc.HttpApiDocDefinitionParser;
import org.apache.shenyu.client.core.register.parser.apimeta.ApiMetaDefinitionParser;
import org.apache.shenyu.client.core.register.parser.apimeta.DefaultApiMetaDefinitionParser;
import org.apache.shenyu.client.core.register.parser.apimeta.DefaultPreApiMetaBeanParser;
import org.apache.shenyu.client.core.register.parser.apimeta.PreApiMetaBeanParser;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiBeansExtractor;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_DOC_BEAN_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_DOC_DEFINITION_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_BEAN_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_DEFINITION_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.PRE_API_META_BEAN_MATCHER;

@Configuration(proxyBeanMethods = false)
@ConditionalOnBean(ClientRegisterConfiguration.class)
public class ShenyuSpringMvcClientInfoRegisterConfiguration {

    public ShenyuSpringMvcClientInfoRegisterConfiguration() {
    }

    /**
     * ClientInfoRefreshedEventListener Bean.
     *
     * @param clientRegisterConfig clientRegisterConfig
     * @param publisher publisher
     * @return clientInfoRefreshedEventListener
     */
    @Bean
    public ClientInfoRefreshedEventListener clientInfoEventListener(final ClientRegisterConfig clientRegisterConfig,
                                                                    final ShenyuClientRegisterEventPublisher publisher) {
        return new ClientInfoRefreshedEventListener(clientRegisterConfig, publisher);
    }

    /**
     * ApiBeansExtractor Bean.
     * @param clientRegisterConfig clientRegisterConfig
     * @return apiBeansExtractor
     */
    @Bean
    @ConditionalOnMissingBean
    public ApiBeansExtractor<Object> apiBeansExtractor(final ClientRegisterConfig clientRegisterConfig) {
        return new SpringMvcApiBeansExtractor(clientRegisterConfig.getContextPath());
    }

    /**
     * ApiMetaBeanMatcher Bean.
     *
     * @return apiMetaBeanMatcher.
     */
    @Bean(name = API_META_BEAN_MATCHER)
    @ConditionalOnMissingBean(name = API_META_BEAN_MATCHER)
    public Matcher<ApiBean<Object>> apiMetaBeanMatcher() {
        return new DefaultApiMetaBeanMatcher<>();
    }

    /**
     * apiDefinitionMetaMatcher Bean.
     *
     * @return apiDefinitionMetaMatcher
     */
    @Bean(name = API_META_DEFINITION_MATCHER)
    @ConditionalOnMissingBean(name = API_META_DEFINITION_MATCHER)
    public Matcher<ApiBean<Object>.ApiDefinition> apiMetaDefinitionMatcher() {
        return new DefaultApiMetaDefinitionMatcher<>();
    }

    /**
     * ApiMetaDefinitionParser Bean.
     *
     * @param clientRegisterConfig clientRegisterConfig
     * @return apiMetaParser
     */
    @Bean
    public ApiMetaDefinitionParser<Object> apiMetaDefinitionParser(final ClientRegisterConfig clientRegisterConfig) {
        return new DefaultApiMetaDefinitionParser<>(clientRegisterConfig);
    }

    /**
     * PreApiMetaBeanMatcher Bean.
     *
     * @return preApiMetaBeanMatcher
     */
    @Bean(name = PRE_API_META_BEAN_MATCHER)
    @ConditionalOnMissingBean(name = PRE_API_META_BEAN_MATCHER)
    public Matcher<ApiBean<Object>> preApiMetaBeanMatcher() {
        return new DefaultPreApiMetaBeanMatcher<>();
    }

    /**
     * preApiBeanMetaParser Bean.
     *
     * @param clientRegisterConfig clientRegisterConfig
     * @return apiBeanMetaParser
     */
    @Bean
    public PreApiMetaBeanParser<Object> preApiBeanMetaParser(final ClientRegisterConfig clientRegisterConfig) {
        return new DefaultPreApiMetaBeanParser<>(clientRegisterConfig);
    }

    /**
     * ApiDocBeanMatcher Bean.
     *
     * @return apiDocBeanMatcher.
     */
    @Bean(name = API_DOC_BEAN_MATCHER)
    @ConditionalOnMissingBean(name = API_DOC_BEAN_MATCHER)
    public Matcher<ApiBean<Object>> apiDocBeanMatcher() {
        return new DefaultApiDocBeanMatcher<>();
    }

    /**
     * ApiDocDefinitionMatcher Bean.
     *
     * @return apiDocDefinitionMatcher
     */
    @Bean(name = API_DOC_DEFINITION_MATCHER)
    @ConditionalOnMissingBean(name = API_DOC_DEFINITION_MATCHER)
    public Matcher<ApiBean<Object>.ApiDefinition> apiDocDefinitionMatcher() {
        return new DefaultApiDocDefinitionMatcher<>();
    }

    /**
     * ApiDocDefinitionParser Bean.
     *
     * @param clientRegisterConfig clientRegisterConfig
     * @return apiDocDefinitionParser
     */
    @Bean
    @ConditionalOnMissingBean
    public ApiDocDefinitionParser<Object> apiDocDefinitionParser(final ClientRegisterConfig clientRegisterConfig) {
        return new HttpApiDocDefinitionParser(clientRegisterConfig);
    }

    /**
     * ClientRegisterConfig Bean.
     * @param shenyuClientConfig shenyuClientConfig
     * @param applicationContext applicationContext
     * @param env env
     * @return clientRegisterConfig
     */
    @Bean
    public ClientRegisterConfig clientRegisterConfig(final ShenyuClientConfig shenyuClientConfig,
                                                     final ApplicationContext applicationContext,
                                                     final Environment env) {
        return new ClientRegisterConfigImpl(shenyuClientConfig, RpcTypeEnum.HTTP, applicationContext, env);
    }
}
