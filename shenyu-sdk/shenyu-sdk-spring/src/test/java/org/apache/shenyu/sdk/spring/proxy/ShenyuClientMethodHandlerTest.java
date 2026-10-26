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
import java.util.Collections;
import java.util.HashMap;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledForJreRange;
import org.junit.jupiter.api.condition.JRE;
import org.springframework.http.HttpStatus;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.HttpClientErrorException;

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

    @Test
    public void handlerResponseAcceptsAllSuccessfulStatuses() {
        assertEquals(42, handlerResponse(200, "42", Integer.class));
        assertEquals(42, handlerResponse(201, "42", Integer.class));
        assertEquals(42, handlerResponse(202, "42", Integer.class));
        assertNull(handlerResponse(204, "", Integer.class));
    }

    @Test
    public void handlerResponseThrowsForNonSuccessfulStatusesOnTypedMethods() {
        assertThrows(HttpClientErrorException.class, () -> handlerResponse(400, "bad request", Integer.class));
        assertThrows(HttpClientErrorException.class, () -> handlerResponse(500, "server error", Integer.class));
    }

    @Test
    public void handlerResponseValidatesVoidMethods() {
        assertThrows(HttpClientErrorException.class, () -> handlerResponse(500, "", void.class));
        assertNull(handlerResponse(204, "", void.class));
    }

    @Test
    public void handlerResponseReturnsRawResponseRegardlessOfStatus() {
        final ShenyuResponse response = response(500, "server error");
        final ShenyuClientMethodHandler handler = newHandler();
        assertSame(response, ReflectionTestUtils.invokeMethod(handler, "handlerResponse", response, ShenyuResponse.class));
    }

    private Object handlerResponse(final int status, final String body, final Class<?> returnType) {
        return ReflectionTestUtils.invokeMethod(newHandler(), "handlerResponse", response(status, body), returnType);
    }

    private ShenyuClientMethodHandler newHandler() {
        return new ShenyuClientMethodHandler(null, new RequestTemplate(), null, Collections.emptyMap());
    }

    private ShenyuResponse response(final int status, final String body) {
        final ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "http://localhost/test",
                new HashMap<>(), "", "test", new RequestTemplate());
        return new ShenyuResponse(status, HttpStatus.valueOf(status).getReasonPhrase(), Collections.emptyMap(), body, request);
    }

}
