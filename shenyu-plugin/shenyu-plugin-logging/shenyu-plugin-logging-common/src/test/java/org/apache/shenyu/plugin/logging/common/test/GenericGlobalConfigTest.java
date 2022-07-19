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

package org.apache.shenyu.plugin.logging.common.test;

import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for GenericGlobalConfig.
 */
public class GenericGlobalConfigTest {

    @Test
    public void testSetGenericGlobalConfigSampleRate() {
        GenericGlobalConfig genericGlobalConfig = new GenericGlobalConfig();
        genericGlobalConfig.setSampleRate("1");
        Assertions.assertEquals(genericGlobalConfig.getSampleRate(), "1");
    }

    @Test
    public void testSetGenericGlobalConfigMaxResponseBody() {
        GenericGlobalConfig genericGlobalConfig = new GenericGlobalConfig();
        genericGlobalConfig.setMaxResponseBody(5);
        Assertions.assertEquals(genericGlobalConfig.getMaxResponseBody(), 5);
    }

    @Test
    public void testSetGenericGlobalConfigMaxRequestBody() {
        GenericGlobalConfig genericGlobalConfig = new GenericGlobalConfig();
        genericGlobalConfig.setMaxRequestBody(5);
        Assertions.assertEquals(genericGlobalConfig.getMaxRequestBody(), 5);
    }

    @Test
    public void testSetGenericGlobalConfigBufferQueueSize() {
        GenericGlobalConfig genericGlobalConfig = new GenericGlobalConfig();
        genericGlobalConfig.setBufferQueueSize(5000);
        Assertions.assertEquals(genericGlobalConfig.getBufferQueueSize(), 5000);
    }
}
