/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.web.configuration;

import org.dromara.soul.web.result.SoulDefaultResult;
import org.dromara.soul.web.result.SoulResult;
import org.dromara.soul.web.result.SpringBeanUtils;
import org.springframework.beans.BeansException;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;

/**
 * The type Soul result configuration.
 *
 * @author xiaoyu
 */
public class SoulResultConfiguration {

    /**
     * Application context aware application context aware.
     *
     * @return the application context aware
     */
    @Bean
    public ApplicationContextAware applicationContextAware() {
        return new SoulApplicationContextAware();
    }

    /**
     * Soul result soul result.
     *
     * @return the soul result
     */
    @Bean
    @ConditionalOnMissingBean(SoulResult.class)
    public SoulResult soulResult() {
        return new SoulDefaultResult();
    }

    /**
     * The type Soul application context aware.
     */
    public static class SoulApplicationContextAware implements ApplicationContextAware {
        @Override
        public void setApplicationContext(final ApplicationContext applicationContext) throws BeansException {
            SpringBeanUtils.getInstance().setCfgContext((ConfigurableApplicationContext) applicationContext);
        }
    }
}
