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

package org.apache.shenyu.registry.api.config;

import org.junit.jupiter.api.Test;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RegisterConfigTest {

    @Test
    void testDefaultConstructor() {
        RegisterConfig config = new RegisterConfig();
        assertFalse(config.getEnabled());
        assertNull(config.getRegisterType());
        assertNull(config.getServerLists());
        assertNotNull(config.getProps());
    }

    @Test
    void testParameterizedConstructor() {
        Properties props = new Properties();
        props.setProperty("key", "value");

        RegisterConfig config = new RegisterConfig("type", "localhost:8080", props);
        assertTrue(config.getProps().containsKey("key"));
        assertEquals("value", config.getProps().getProperty("key"));
        assertEquals("type", config.getRegisterType());
        assertEquals("localhost:8080", config.getServerLists());
    }

    @Test
    void testSettersAndGetters() {
        RegisterConfig config = new RegisterConfig();
        config.setEnabled(true);
        config.setRegisterType("type");
        config.setServerLists("localhost:8080");

        Properties props = new Properties();
        props.setProperty("key", "value");
        config.setProps(props);

        assertTrue(config.getEnabled());
        assertEquals("type", config.getRegisterType());
        assertEquals("localhost:8080", config.getServerLists());
        assertEquals("value", config.getProps().getProperty("key"));
    }

    @Test
    void testEquals() {
        Properties props1 = new Properties();
        props1.setProperty("key", "value");

        Properties props2 = new Properties();
        props2.setProperty("key", "value");

        RegisterConfig config1 = new RegisterConfig("type", "localhost:8080", props1);
        RegisterConfig config2 = new RegisterConfig("type", "localhost:8080", props2);

        assertEquals(config1, config2);
        assertEquals(config1.hashCode(), config2.hashCode());

        config2.setRegisterType("differentType");
        assertNotEquals(config1, config2);
    }

    @Test
    void testBuilder() {
        Properties props = new Properties();
        props.setProperty("key", "value");

        RegisterConfig config = RegisterConfig.Builder.builder()
                .enabled(true)
                .registerType("type")
                .serverLists("localhost:8080")
                .props(props)
                .build();

        assertTrue(config.getEnabled());
        assertEquals("type", config.getRegisterType());
        assertEquals("localhost:8080", config.getServerLists());
        assertEquals("value", config.getProps().getProperty("key"));
    }
}
