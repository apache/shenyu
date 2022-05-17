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
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;

public class SpringMvcMappingPathControllerTest extends AbstractTest {

    @Test
    public void testHello() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/hello", java.lang.String.class);
        assertEquals("hello! I'm Shenyu-Gateway System. Welcome!", res);
    }

    @Test
    public void testHi()throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/hi?name=tom", java.lang.String.class);
        assertEquals("hi! tom! I'm Shenyu-Gateway System. Welcome!", res);
    }

    @Test
    public void testPost()throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/post/hi?name=tom", java.lang.String.class);
        assertEquals("[post method result]:hi! tom! I'm Shenyu-Gateway System. Welcome!", res);
    }
}
