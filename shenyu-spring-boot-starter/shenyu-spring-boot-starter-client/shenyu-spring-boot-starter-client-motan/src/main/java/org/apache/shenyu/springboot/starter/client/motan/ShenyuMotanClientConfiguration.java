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
import com.weibo.api.motan.config.springsupport.ProtocolConfigBean;
import com.weibo.api.motan.config.springsupport.RegistryConfigBean;
import com.weibo.api.motan.util.MotanSwitcherUtil;
import org.apache.shenyu.client.motan.MotanServiceEventListener;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.springboot.starter.client.common.config.ShenyuClientCommonBeanConfiguration;
import org.apache.shenyu.springboot.starter.client.motan.property.RegistryConfig;
import org.apache.shenyu.springboot.starter.client.motan.property.ShenyuMotanConfig;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.ImportAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * Motan type client event listener.
 */
@Configuration
@ImportAutoConfiguration(ShenyuClientCommonBeanConfiguration.class)
@ConditionalOnProperty(value = "shenyu.register.enabled", matchIfMissing = true, havingValue = "true")
public class ShenyuMotanClientConfiguration implements ApplicationListener<ContextRefreshedEvent> {

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
     * Motan registry.
     *
     * @return registryProperties
     */
    @Bean
    @ConfigurationProperties(prefix = "motan.registry")
    public RegistryConfig registryProperties() {
        return new RegistryConfig();
    }

    /**
     * Shenyu motan properties.
     *
     * @return shenyuMotanProperties
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.client.motan")
    public ShenyuMotanConfig shenyuMotanProperties() {
        return new ShenyuMotanConfig();
    }

    /**
     * Define the annotation bean, set the scan package.
     *
     * <p>The packagePath seems to be better set in ShenyuMotanConfig,
     * but if so, the parameters in the configuration file can not be read,
     * using @Value can, probably related to the bean loading timing.
     *
     * @param packagePath package path
     * @return  annotationBean
     */

    @Bean
    public AnnotationBean motanAnnotationBean(@Value("${shenyu.client.motan.package-path}")final String packagePath) {
        AnnotationBean motanAnnotationBean = new AnnotationBean();
        motanAnnotationBean.setPackage(packagePath);
        return motanAnnotationBean;
    }

    /**
     * Define a bean with name of ProtocolConfigBean.
     *
     * <p>The Default motan service name is "motan2"
     *
     * @param shenyuMotanConfig shenyu motan shenyuMotanConfig
     * @return ProtocolConfigBean
     */
    @Bean("motan2")
    public ProtocolConfigBean protocolConfig(final ShenyuMotanConfig shenyuMotanConfig) {
        ProtocolConfigBean config = new ProtocolConfigBean();
        config.setDefault(shenyuMotanConfig.getProtocol().isDefault());
        config.setName(shenyuMotanConfig.getProtocol().getName());
        config.setMaxContentLength(shenyuMotanConfig.getProtocol().getMaxContentLength());
        return config;
    }

    /**
     * Define a bean with name registryConfig1 of RegistryConfigBean.
     *
     * @param registryConfig  registry registryConfig
     * @return registryConfig
     */
    @Bean
    public RegistryConfigBean registryConfig(final RegistryConfig registryConfig) {
        RegistryConfigBean config = new RegistryConfigBean();
        config.setRegProtocol(registryConfig.getProtocol());
        config.setAddress(registryConfig.getAddress());
        return config;
    }

    /**
     * Define a bean with name baseServiceConfig of BasicServiceConfigBean.
     *
     * @param shenyuMotanConfig shenyu motan shenyuMotanConfig
     * @return baseServiceConfig
     */
    @Bean
    public BasicServiceConfigBean baseServiceConfig(final ShenyuMotanConfig shenyuMotanConfig) {
        BasicServiceConfigBean config = new BasicServiceConfigBean();
        config.setExport(String.format("%s:%s", shenyuMotanConfig.getProtocol().getName(), shenyuMotanConfig.getBasicServiceConfig().getExportPort()));
        config.setGroup(shenyuMotanConfig.getBasicServiceConfig().getGroup());
        config.setAccessLog(shenyuMotanConfig.getBasicServiceConfig().isAccessLog());
        config.setShareChannel(shenyuMotanConfig.getBasicServiceConfig().isShareChannel());
        config.setModule(shenyuMotanConfig.getBasicServiceConfig().getModule());
        config.setApplication(shenyuMotanConfig.getBasicServiceConfig().getApplication());
        config.setRegistry(shenyuMotanConfig.getBasicServiceConfig().getRegistry());
        config.setRequestTimeout(shenyuMotanConfig.getBasicServiceConfig().getRequestTimeout());
        return config;
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        ShenyuMotanConfig shenyuMotanConfig = event.getApplicationContext().getBean(ShenyuMotanConfig.class);
        MotanSwitcherUtil.setSwitcherValue(MotanConstants.REGISTRY_HEARTBEAT_SWITCHER, shenyuMotanConfig.getBasicServiceConfig().getRegistryHeartBeatSwitcher());
    }
}
