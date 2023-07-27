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

package org.apache.shenyu.sdk.spring.proxy;

import java.io.IOException;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import org.springframework.http.HttpStatus;

/**
 * {@link ShenyuClientInvocationHandler} test.
 */
public class ShenyuClientInvocationHandlerTest extends AbstractProxyTest {

    @Test
    public void registerBeanDefinitionsTest() throws IllegalAccessException, IOException {
        init();

        assertEquals(InvocationClient.class.getMethods().length, getMap().size(), "shenyu client handle method error.");

        ShenyuResponse response = new ShenyuResponse(HttpStatus.OK.value(), "", null, JsonUtils.toJson(getMetaData()), null);
        when(getClient().execute(any(ShenyuRequest.class))).thenReturn(response);

        assertDoesNotThrow(() -> getTopClient().findById("id"));

        response = new ShenyuResponse(HttpStatus.OK.value(), "", null, "32", null);
        when(getClient().execute(any(ShenyuRequest.class))).thenReturn(response);
        final Integer insert = getTopClient().insert(getMetaData());
        assertEquals(insert, 32);
    }

}
