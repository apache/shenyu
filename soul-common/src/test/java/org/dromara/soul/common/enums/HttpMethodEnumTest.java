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

package org.dromara.soul.common.enums;

import org.dromara.soul.common.exception.SoulException;
import org.hamcrest.CoreMatchers;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;


/**
 * Test Cases for HttpMethodEnum.
 *
 * @author tangtian
 */
public class HttpMethodEnumTest {

    @Test
    public void testAcquireByNameValid() {
        Arrays.stream(HttpMethodEnum.values()).forEach(
            e -> {
                if (e.getSupport()) {
                    HttpMethodEnum.acquireByName(e.getName());
                }
            }
        );
    }

    @Test
    public void testAcquireByNameInvalid() {
        String httpMethodName = "InvalidName";
        try {
            HttpMethodEnum.acquireByName(httpMethodName);
            fail();
        } catch (SoulException expected) {
            assertThat(expected.getMessage(),
                    CoreMatchers.containsString("this http method can not support"));
        }
    }
}
