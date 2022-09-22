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

package org.apache.shenyu.register.client.http.utils;

import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link SystemUtils}.
 */
class SystemUtilsTest {

    @Test
    void getOsNameTest() {
        final Properties properties = System.getProperties();
        try {
            final Optional<String> osName = SystemUtils.getOsName();
            if (osName.isPresent() && !osName.get().toLowerCase().contains("mac")) {
                final Properties mock = mock(Properties.class);
                System.setProperties(mock);
                when(mock.getProperty(anyString())).thenThrow(SecurityException.class);
                SystemUtils.getOsName();
            }
        } catch (Exception e) {
            System.setProperties(properties);
        } finally {
            System.setProperties(properties);
        }
    }
}
