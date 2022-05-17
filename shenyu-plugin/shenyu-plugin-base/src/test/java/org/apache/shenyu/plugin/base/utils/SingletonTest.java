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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.utils.Singleton;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The Singleton test.
 */
@ExtendWith(MockitoExtension.class)
public final class SingletonTest {

    private SingletonTest singletonTest;

    @BeforeEach
    public void setUp() {
        singletonTest = new SingletonTest();
        Singleton.INST.single(SingletonTest.class, singletonTest);
    }

    /**
     * The single test.
     */
    @Test
    public void singleTest() {
        ConditionData conditionData = new ConditionData();
        Singleton.INST.single(ConditionData.class, conditionData);
        assertEquals(conditionData, Singleton.INST.get(ConditionData.class));
    }

    /**
     * The get test.
     */
    @Test
    public void getTest() {
        assertEquals(singletonTest, Singleton.INST.get(SingletonTest.class));
    }
}
