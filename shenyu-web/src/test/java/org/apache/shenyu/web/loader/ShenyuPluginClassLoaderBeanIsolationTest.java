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

package org.apache.shenyu.web.loader;

import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.Test;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.stereotype.Component;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShenyuPluginClassLoaderBeanIsolationTest {

    @Test
    void closingOnePluginShouldNotDestroyAnotherPluginsBean() {
        GenericApplicationContext context = new GenericApplicationContext();
        context.refresh();
        SpringBeanUtils.getInstance().setApplicationContext(context);
        ShenyuPluginClassLoader firstLoader = createLoader("plugin-one", FirstPlugin.SharedComponent.class);
        ShenyuPluginClassLoader secondLoader = createLoader("plugin-two", SecondPlugin.SharedComponent.class);

        firstLoader.loadUploadedJarPlugins();
        secondLoader.loadUploadedJarPlugins();
        String firstBeanName = "plugin-one#" + FirstPlugin.SharedComponent.class.getName();
        String secondBeanName = "plugin-two#" + SecondPlugin.SharedComponent.class.getName();
        assertTrue(context.containsBean(firstBeanName));
        assertTrue(context.containsBean(secondBeanName));

        firstLoader.close();

        assertFalse(context.containsBean(firstBeanName));
        assertTrue(context.containsBean(secondBeanName));
        secondLoader.close();
        context.close();
    }

    private ShenyuPluginClassLoader createLoader(final String path, final Class<?> pluginClass) {
        PluginJarParser.PluginJar pluginJar = new PluginJarParser.PluginJar();
        pluginJar.setAbsolutePath(path);
        pluginJar.setClazzMap(Collections.singletonMap(pluginClass.getName(), new byte[0]));
        return new ShenyuPluginClassLoader(pluginJar);
    }

    private static final class FirstPlugin {

        @Component
        public static final class SharedComponent {
        }
    }

    private static final class SecondPlugin {

        @Component
        public static final class SharedComponent {
        }
    }
}
