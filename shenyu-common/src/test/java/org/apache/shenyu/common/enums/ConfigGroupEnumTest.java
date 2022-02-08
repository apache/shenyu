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

package org.apache.shenyu.common.enums;

import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test Cases for ConfigGroupEnum.
 */
public final class ConfigGroupEnumTest {

    @Test
    public void testAcquireByName() {
        assertThat(ConfigGroupEnum.APP_AUTH, equalTo(ConfigGroupEnum.acquireByName(ConfigGroupEnum.APP_AUTH.name())));
        assertThat(ConfigGroupEnum.APP_AUTH, not(ConfigGroupEnum.acquireByName(ConfigGroupEnum.PLUGIN.name())));

        Exception exception = assertThrows(ShenyuException.class, () -> ConfigGroupEnum.acquireByName("unknown"));
        assertEquals(" this ConfigGroupEnum can not support unknown", exception.getMessage());
    }
}
