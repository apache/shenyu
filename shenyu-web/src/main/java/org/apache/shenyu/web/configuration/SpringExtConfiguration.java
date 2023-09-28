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

package org.apache.shenyu.web.configuration;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.lang.NonNull;

/**
 * The type spring ext configuration.
 */
@Configuration
public class SpringExtConfiguration {

    /**
     * Application context aware application context aware.
     *
     * @return the application context aware
     */
    @Bean
    public ApplicationContextAware applicationContextAware() {
        return new ShenyuApplicationContextAware();
    }

    /**
     * The type shenyu application context aware.
     */
    public static class ShenyuApplicationContextAware implements ApplicationContextAware {

        @Override
        public void setApplicationContext(@NonNull final ApplicationContext applicationContext) throws BeansException {
            SpringBeanUtils.getInstance().setApplicationContext(applicationContext);
            ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
            Singleton.INST.single(ShenyuConfig.class, shenyuConfig);
        }
    }
}
