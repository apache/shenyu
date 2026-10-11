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

package org.apache.shenyu.integrated.test.http;

import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SpringMvcMappingPathControllerTest extends AbstractTest {

    private static final String MULTI_PATH_SUFFIX = "I'm Shenyu-Gateway System. Welcome!";

    @BeforeAll
    static void waitForMultiPathRoutes() throws InterruptedException {
        // Multi-path routes are registered asynchronously; poll until available
        for (int i = 0; i < 30; i++) {
            try {
                String res = HttpHelper.INSTANCE.postGateway("/http/multipath/v1/greet", String.class);
                if (("hello from multipath! " + MULTI_PATH_SUFFIX).equals(res)) {
                    return;
                }
            } catch (IOException e) {
                // route not ready yet, keep waiting
            }
            Thread.sleep(TimeUnit.SECONDS.toMillis(2));
        }
        throw new AssertionError("Multi-path routes not ready after 60s: /http/multipath/v1/greet");
    }

    @Test
    void testHello() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/hello", String.class);
        assertEquals("hello! I'm Shenyu-Gateway System. Welcome!", res);
    }

    @Test
    void testHi() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/hi?name=tom", String.class);
        assertEquals("hi! tom! I'm Shenyu-Gateway System. Welcome!", res);
    }

    @Test
    void testPost() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/post/hi?name=tom", String.class);
        assertEquals("[post method result]:hi! tom! I'm Shenyu-Gateway System. Welcome!", res);
    }

    @Test
    void testMultiPathV1Greet() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/multipath/v1/greet", String.class);
        assertEquals("hello from multipath! " + MULTI_PATH_SUFFIX, res);
    }

    @Test
    void testMultiPathV2Greet() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/multipath/v2/greet", String.class);
        assertEquals("hello from multipath! " + MULTI_PATH_SUFFIX, res);
    }

    @Test
    void testMultiPathV1Echo() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/multipath/v1/echo?name=shenyu", String.class);
        assertEquals("echo: shenyu! " + MULTI_PATH_SUFFIX, res);
    }

    @Test
    void testMultiPathV2Echo() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/multipath/v2/echo?name=shenyu", String.class);
        assertEquals("echo: shenyu! " + MULTI_PATH_SUFFIX, res);
    }
}
