/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.common.http;

import org.junit.Assert;
import org.junit.Test;

public class URLTest {


    @Test
    public void parse() {
        String s = "http://127.0.0.1:8080/proxy/abc?abc=1&cdf=2";
        URL parse = URL.parse(s);
        Assert.assertEquals(parse.getFull(), s);
        Assert.assertEquals(parse.getHost(), "127.0.0.1");
        Assert.assertEquals(parse.getPath(), "/proxy/abc");
        Assert.assertEquals(parse.getProtocol(), "http");
        Assert.assertEquals(parse.getPort(), Integer.valueOf(8080));
        Assert.assertEquals(parse.getParameters().size(), 2);
        Assert.assertEquals(parse.getParameters().get("abc"), "1");
        Assert.assertEquals(parse.getParameters().get("cdf"), "2");
    }

    @Test
    public void testParse2() {
        String s = "dubbo://127.0.0.1:8080/proxy/abc?abc=1&cdf=2";
        URL parse = URL.parse(s);
        Assert.assertEquals(parse.getFull(), s);
        Assert.assertEquals(parse.getHost(), "127.0.0.1");
        Assert.assertEquals(parse.getPath(), "/proxy/abc");
        Assert.assertEquals(parse.getProtocol(), "http");
        Assert.assertEquals(parse.getPort(), Integer.valueOf(8080));
        Assert.assertEquals(parse.getParameters().size(), 2);
        Assert.assertEquals(parse.getParameters().get("abc"), "1");
        Assert.assertEquals(parse.getParameters().get("cdf"), "2");
    }
}
