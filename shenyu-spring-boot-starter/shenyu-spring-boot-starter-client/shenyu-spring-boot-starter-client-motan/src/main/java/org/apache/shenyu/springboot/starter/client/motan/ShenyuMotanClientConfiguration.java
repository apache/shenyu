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

package org.apache.shenyu.springboot.starter.client.motan;

import com.weibo.api.motan.common.MotanConstants;
import com.weibo.api.motan.config.springsupport.AnnotationBean;
import com.weibo.api.motan.config.springsupport.BasicServiceConfigBean;
import com.weibo.api.motan.config.springsupport.RegistryConfigBean;
import com.weibo.api.motan.util.MotanSwitcherUtil;
import org.apache.shenyu.client.motan.MotanServiceEventListener;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.apache.shenyu.springboot.starter.client.motan.property.RegistryProperties;
import org.apache.shenyu.springboot.starter.client.motan.property.ShenyuMotanProperties;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Motan type client event listener.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuMotanClientConfiguration implements ApplicationRunner {

    /**
     * Motan service event listener.
     *
     * @param clientConfig the client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     * @return the motan service event listener
     */
    @Bean
    public MotanServiceEventListener motanServiceEventListener(final ShenyuClientConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        return new MotanServiceEventListener(clientConfig.getClient().get(RpcTypeEnum.MOTAN.getName()), shenyuClientRegisterRepository);
    }

    /**
     * motan registry.
     *
     * @return registryProperties
     */
    @Bean
    @ConfigurationProperties(prefix = "motan.registry")
    public RegistryProperties registryProperties() {
        return new RegistryProperties();
    }

    /**
     * shenyu motan properties.
     *
     * @return shenyuMotanProperties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.client.motan")
    public ShenyuMotanProperties shenyuMotanProperties() {
        return new ShenyuMotanProperties();
    }

    /**
     * define the annotation bean, set the scan package.
     * @param packagePath package path
     * @return  annotationBean
     */

    @Bean
    @Value("${shenyu.client.motan.packagePath:org.apache.shenyu.examples.motan.service}")
    public AnnotationBean motanAnnotationBean(final String packagePath) {
        AnnotationBean motanAnnotationBean = new AnnotationBean();
        motanAnnotationBean.setPackage(packagePath);
        return motanAnnotationBean;
    }

    /**
     * define a bean with name registryConfig1 of RegistryConfigBean.
     * @param properties  registry properties
     * @return registryConfig1
     */
    @Bean
    public RegistryConfigBean registryConfig(final RegistryProperties properties) {
        RegistryConfigBean config = new RegistryConfigBean();
        config.setRegProtocol(properties.getProtocol());
        config.setAddress(properties.getAddress());
        return config;
    }

    /**
     * define a bean with name baseServiceConfig of BasicServiceConfigBean.
     *
     * @param properties shenyu motan properties
     * @return baseServiceConfig
     */
    @Bean
    public BasicServiceConfigBean baseServiceConfig(final ShenyuMotanProperties properties) {
        BasicServiceConfigBean config = new BasicServiceConfigBean();
        config.setExport(properties.getBasicServiceConfig().getExport());
        config.setGroup(properties.getBasicServiceConfig().getGroup());
        config.setAccessLog(properties.getBasicServiceConfig().isAccessLog());
        config.setShareChannel(properties.getBasicServiceConfig().isShareChannel());
        config.setModule(properties.getBasicServiceConfig().getModule());
        config.setApplication(properties.getBasicServiceConfig().getApplication());
        config.setRegistry(properties.getBasicServiceConfig().getRegistry());
        config.setRequestTimeout(properties.getBasicServiceConfig().getRequestTimeout());
        return config;
    }

    @Override
    public void run(final ApplicationArguments args) throws Exception {
        MotanSwitcherUtil.setSwitcherValue(MotanConstants.REGISTRY_HEARTBEAT_SWITCHER, true);
    }
}
