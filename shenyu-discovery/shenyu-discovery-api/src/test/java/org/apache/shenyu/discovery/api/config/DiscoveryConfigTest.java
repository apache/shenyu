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

package org.apache.shenyu.discovery.api.config;

import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Properties;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * The Test Case For {@link DiscoveryConfig}.
 */
@ExtendWith(MockitoExtension.class)
public class DiscoveryConfigTest {

    private final DiscoveryConfig discoveryConfig = new DiscoveryConfig();

    @Test
    public void testDiscoveryConfigProperties() {
        // the discovery name
        String name = "divide_default_discovery";
        // zookeeper nacos etcd consul
        String type = "local";
        // the discovery pops (json)
        String serverList = "{\"host\": \"localhost\"}";  

        testGetSet(DiscoveryConfig::getName, DiscoveryConfig::setName, name);
        testGetSet(DiscoveryConfig::getType, DiscoveryConfig::setType, type);
        testGetSet(DiscoveryConfig::getServerList, DiscoveryConfig::setServerList, serverList);

        Properties properties = Mockito.mock(Properties.class);
        Assertions.assertNotNull(discoveryConfig.getProps());
        discoveryConfig.setProps(properties);
        Assertions.assertEquals(properties, discoveryConfig.getProps());
    }

    /**
     * abstract function to test properties get and set.
     *
     * @param getter Object Get Function
     * @param setter Object Set Function
     * @param value  Object Function Value
     * @param <T>    T
     */
    private <T> void testGetSet(final Function<DiscoveryConfig, T> getter,
                                final BiConsumer<DiscoveryConfig, T> setter,
                                final T value) {
        Assertions.assertNull(getter.apply(discoveryConfig));
        setter.accept(discoveryConfig, value);
        Assertions.assertEquals(getter.apply(discoveryConfig), value);
    }
}
