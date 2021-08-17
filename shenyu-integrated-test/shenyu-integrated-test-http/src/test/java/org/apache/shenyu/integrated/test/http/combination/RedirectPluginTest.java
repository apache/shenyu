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

package org.apache.shenyu.integrated.test.http.combination;

import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.Test;

import java.io.IOException;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public final class RedirectPluginTest extends AbstractTest {

    @Test
    public void test() throws IOException {
        final String redirectUserId = "111";
        final String redirectToInnerPathUrl = "/http/test/path/" + redirectUserId + "?name=redirectToInnerPath";
        Map<String, Object> result = HttpHelper.INSTANCE.getFromGateway(redirectToInnerPathUrl, Map.class);
        assertNotNull(result);
        assertNull(result.get("userId"));
        assertEquals("UP", result.get("status"));

        final String redirectToOuterPathUrl = "/http/test/path/" + redirectUserId + "?name=redirectToOuterPath";
        result = HttpHelper.INSTANCE.getFromGateway(redirectToOuterPathUrl, Map.class);
        assertNotEquals(redirectUserId, result.get("userId"));
        assertEquals("222", result.get("userId"));

        final String notRedirectUserId = "333";
        final String notRedirectUrl = "/http/test/path/" + notRedirectUserId + "?name=notRedirect";
        result = HttpHelper.INSTANCE.getFromGateway(notRedirectUrl, Map.class);
        assertNotNull(result);
        assertEquals(notRedirectUserId, result.get("userId"));
    }
}
