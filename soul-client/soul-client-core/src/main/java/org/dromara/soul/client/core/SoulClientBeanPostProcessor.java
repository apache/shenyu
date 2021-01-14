/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.client.core;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.core.config.SoulClientConfig;
import org.dromara.soul.client.core.processor.SoulClientProcessChecker;
import org.dromara.soul.spi.ExtensionLoader;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;

/**
 * Demo bean post processor.
 *
 * @author tydhot
 */
@Slf4j
public class SoulClientBeanPostProcessor implements BeanPostProcessor {
    private final SoulClientProcessChecker soulClientProcessChecker;

    private final SoulClientConfig soulClientConfig;

    public SoulClientBeanPostProcessor(final SoulClientConfig soulClientConfig) {
        this.soulClientConfig = soulClientConfig;
        this.soulClientProcessChecker = ExtensionLoader.getExtensionLoader(SoulClientProcessChecker.class).getJoin(soulClientConfig.getType());
        SoulClientRegisterEventPublisher.getInstance().start(soulClientConfig);
    }

    @Override
    public Object postProcessAfterInitialization(final Object bean, final String beanName) throws BeansException {
        if (soulClientProcessChecker.checkProcess(bean, soulClientConfig)) {
            SoulClientRegisterEventPublisher.getInstance().publishEvent(bean);
        }
        return bean;
    }
}
