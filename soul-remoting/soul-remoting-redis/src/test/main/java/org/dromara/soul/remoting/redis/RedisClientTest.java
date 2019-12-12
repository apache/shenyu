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

package org.dromara.soul.remoting.redis;

import org.dromara.soul.common.http.URL;
import org.dromara.soul.remoting.redis.operation.ValueOperation;
import org.junit.Before;

/**
 */
public class RedisClientTest {

    private URL url;

    private RedisClient<String, String> client;
    @Before
    public void setUrl() {
        String urlStr = "redis://192.168.1.82:6379?&mode=default";
        this.url = URL.parse(urlStr);
        client = new RedisClient<>(url);
    }

    @org.junit.Test
    public void getUrl() {
    }

    @org.junit.Test
    public void mapOperation() {
    }

    @org.junit.Test
    public void listOperation() {
    }

    @org.junit.Test
    public void setOperation() {
    }

    @org.junit.Test
    public void valueOperation() {
        ValueOperation valueOperation1 = client.valueOperation();
        ValueOperation<String, String> valueOperation = valueOperation1;
        valueOperation.set("test_123456", "test_456");
        String value = valueOperation.get("test_123456", String.class);
        System.out.println(value);
    }

    @org.junit.Test
    public void zsetOperation() {
    }

    @org.junit.Test
    public void scriptOperation() {
    }
}