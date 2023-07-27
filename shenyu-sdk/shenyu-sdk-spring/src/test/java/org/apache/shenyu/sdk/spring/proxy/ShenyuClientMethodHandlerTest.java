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
import java.util.Arrays;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import static org.junit.jupiter.api.Assertions.assertAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledForJreRange;
import org.junit.jupiter.api.condition.JRE;

/**
 * {@link ShenyuClientMethodHandler} test.
 */
public class ShenyuClientMethodHandlerTest extends AbstractProxyTest {

    @Test
    @DisabledForJreRange(min = JRE.JAVA_16)
    public void handlerTest() throws IOException, IllegalAccessException {
        init();

        assertAll(getMap().values().stream().map(handler -> () -> {
            final RequestTemplate requestTemplate = (RequestTemplate) REQUEST_TEMPLATE.get(handler);
            if (!Arrays.asList(ShenyuRequest.HttpMethod.values()).contains(requestTemplate.getHttpMethod())) {
                throw new IllegalStateException("shenyu client path transform to request error");
            }
        }));
    }

}
