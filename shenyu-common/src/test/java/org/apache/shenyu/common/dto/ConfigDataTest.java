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

package org.apache.shenyu.common.dto;

import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Test case for ConfigData.
 */
public class ConfigDataTest {

    private static final int HASHVALUE = 0;

    private static final long LAST_MODIFY_TIME = System.currentTimeMillis();

    @Test
    public void testToString() {
        ConfigData<Object> configData = new ConfigData<>();
        configData.setLastModifyTime(LAST_MODIFY_TIME);
        configData.setHashValue(HASHVALUE);
        configData.setData(Collections.EMPTY_LIST);
        assertNotNull(configData.toString());
    }

    /**
     * just improve code coverage.
     */
    @Test
    public void testGetterSetter() {
        ConfigData<Object> configData = new ConfigData<>(HASHVALUE, LAST_MODIFY_TIME, Collections.EMPTY_LIST);
        assertEquals(configData.getData(), Collections.EMPTY_LIST);
        assertEquals(configData.getHashValue(), HASHVALUE);
        assertEquals(configData.getLastModifyTime(), LAST_MODIFY_TIME);
    }

}
