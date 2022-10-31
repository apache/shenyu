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

package org.apache.shenyu.web.logo;

import org.junit.jupiter.api.Test;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.context.event.ApplicationEnvironmentPreparedEvent;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.mock.env.MockEnvironment;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The TestCase for ShenyuLogo.
 */
public final class ShenyuLogoTest {

    private final ShenyuLogo shenyuLogo = new ShenyuLogo();

    @Test
    public void testBuildBannerText() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method method = shenyuLogo.getClass().getDeclaredMethod("buildBannerText", (Class<?>[]) null);
        method.setAccessible(true);
        Object logInfo = method.invoke(shenyuLogo);
        assertTrue(logInfo instanceof String);
    }

    @Test
    public void testOnApplicationEvent() throws NoSuchFieldException, IllegalAccessException {
        SpringApplication application = new SpringApplication();
        ConfigurableEnvironment environment = new MockEnvironment();
        ApplicationEnvironmentPreparedEvent event = new ApplicationEnvironmentPreparedEvent(null, application, null, environment);
        shenyuLogo.onApplicationEvent(event);
        shenyuLogo.onApplicationEvent(event);
        Field field = shenyuLogo.getClass().getDeclaredField("ALREADY_LOG");
        field.setAccessible(true);
        AtomicBoolean atomicBoolean = (AtomicBoolean) field.get(shenyuLogo);
        assertTrue(atomicBoolean.get());
    }

}
