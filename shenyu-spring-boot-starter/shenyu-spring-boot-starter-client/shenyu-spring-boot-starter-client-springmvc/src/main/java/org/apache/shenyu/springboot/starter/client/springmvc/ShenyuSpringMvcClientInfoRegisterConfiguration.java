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
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientInfoRefreshedEventListener;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.apache.shenyu.client.core.register.matcher.Matcher;
import org.apache.shenyu.client.core.register.parser.ApiDocDefinitionParser;
import org.apache.shenyu.client.core.register.parser.ApiMetaDefinitionParser;
import org.apache.shenyu.client.core.register.parser.PreApiMetaBeanParser;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiBeansExtractor;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiDefinitionMetaMatcher;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiMetaBeanMatcher;
import org.apache.shenyu.client.springmvc.register.SpringMvcApiMetaDefinitionParser;
import org.apache.shenyu.client.springmvc.register.SpringMvcPreApiMetaBeanMatcher;
import org.apache.shenyu.client.springmvc.register.SpringMvcPreApiMetaBeanParser;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.Optional;
import java.util.Properties;

import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_DOC_BEAN_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_DOC_DEFINITION_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_BEAN_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.API_META_DEFINITION_MATCHER;
import static org.apache.shenyu.client.core.constant.ShenyuClientConstants.PRE_API_META_BEAN_MATCHER;

@Configuration(proxyBeanMethods = false)
@ConditionalOnBean(ClientRegisterConfiguration.class)
public class ShenyuSpringMvcClientInfoRegisterConfiguration {

    private final boolean addPrefixed;

    private final boolean isFull;

    private final String contextPath;

    private final String appName;

    private final PropertiesConfig clientConfig;

    public ShenyuSpringMvcClientInfoRegisterConfiguration(final ShenyuClientConfig clientConfig) {

        this.clientConfig = clientConfig.getClient().get(RpcTypeEnum.HTTP.getName());

        Properties props = this.clientConfig.getProps();

        this.addPrefixed = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.ADD_PREFIXED,
                Boolean.FALSE.toString()));

        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));

        this.contextPath = Optional.ofNullable(props
                .getProperty(ShenyuClientConstants.CONTEXT_PATH))
                .map(UriUtils::repairData).orElse("");

        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);
    }

    /**
     * ClientInfoRefreshedEventListener Bean.
     *
     * @param publisher publisher
     * @return clientInfoRefreshedEventListener
     */
    @Bean
    public ClientInfoRefreshedEventListener clientInfoEventListener(final ShenyuClientRegisterEventPublisher publisher) {
        return new ClientInfoRefreshedEventListener(clientConfig, publisher, RpcTypeEnum.HTTP);
    }

    /**
     * ApiBeansExtractor Bean.
     *
     * @return apiBeansExtractor
     */
    @Bean
    @ConditionalOnMissingBean
    public ApiBeansExtractor<Object> apiBeansExtractor() {
        return new SpringMvcApiBeansExtractor(contextPath);
    }

    /**
     * ApiMetaBeanMatcher Bean.
     *
     * @return apiMetaBeanMatcher.
     */
    @Bean(name = API_META_BEAN_MATCHER)
    @ConditionalOnMissingBean(name = API_META_BEAN_MATCHER)
    public Matcher<ApiBean<Object>> apiMetaBeanMatcher() {
        return new SpringMvcApiMetaBeanMatcher();
    }

    /**
     * apiDefinitionMetaMatcher Bean.
     *
     * @return apiDefinitionMetaMatcher
     */
    @Bean(name = API_META_DEFINITION_MATCHER)
    @ConditionalOnMissingBean(name = API_META_DEFINITION_MATCHER)
    public Matcher<ApiBean<Object>.ApiDefinition> apiDefinitionMetaMatcher() {
        return new SpringMvcApiDefinitionMetaMatcher();
    }

    /**
     * ApiMetaDefinitionParser Bean.
     *
     * @return apiMetaParser
     */
    @Bean
    public ApiMetaDefinitionParser<Object> apiMetaDefinitionParser() {
        return new SpringMvcApiMetaDefinitionParser(addPrefixed, appName);
    }

    /**
     * PreApiMetaBeanMatcher Bean.
     *
     * @return preApiMetaBeanMatcher
     */
    @Bean(name = PRE_API_META_BEAN_MATCHER)
    @ConditionalOnMissingBean(name = PRE_API_META_BEAN_MATCHER)
    public Matcher<ApiBean<Object>> preApiMetaBeanMatcher() {
        return new SpringMvcPreApiMetaBeanMatcher();
    }

    /**
     * apiBeanMetaParser Bean.
     *
     * @return apiBeanMetaParser
     */
    @Bean
    public PreApiMetaBeanParser<Object> apiBeanMetaParser() {
        return new SpringMvcPreApiMetaBeanParser(addPrefixed, appName);
    }

    /**
     * ApiDocBeanMatcher Bean.
     *
     * @return apiDocBeanMatcher.
     */
    @Bean(name = API_DOC_BEAN_MATCHER)
    @ConditionalOnMissingBean(name = API_DOC_BEAN_MATCHER)
    public Matcher<ApiBean<Object>> apiDocBeanMatcher() {
        //todo implements spring mvc doc collection
        return e -> false;
    }

    /**
     * ApiDocDefinitionMatcher Bean.
     *
     * @return apiDocDefinitionMatcher
     */
    @Bean(name = API_DOC_DEFINITION_MATCHER)
    @ConditionalOnMissingBean(name = API_DOC_DEFINITION_MATCHER)
    public Matcher<ApiBean<Object>.ApiDefinition> apiDocDefinitionMatcher() {
        //todo implements spring mvc doc collection
        return e -> false;
    }

    /**
     * ApiDocDefinitionParser Bean.
     *
     * @return apiDocDefinitionParser
     */
    @Bean
    @ConditionalOnMissingBean
    public ApiDocDefinitionParser<Object> apiDocDefinitionParser() {
        //todo implements spring mvc doc collection
        return t -> Collections.emptyList();
    }
}
