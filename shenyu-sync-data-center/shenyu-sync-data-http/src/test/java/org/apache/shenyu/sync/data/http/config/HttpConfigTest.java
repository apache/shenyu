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

package org.apache.shenyu.sync.data.http.config;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.util.Objects;

/**
 * add test case for {@link HttpConfig}.
 */
public class HttpConfigTest {

    private static final String URL = "url";

    private static final Integer DELAY_TIME = 100;

    private static final Integer CONNECTION_TIMEOUT = 1000;
    
    private HttpConfig httpConfig;

    private HttpConfig that;

    @BeforeEach
    public void setUp() {
        httpConfig = new HttpConfig();
        httpConfig.setUrl(URL);
        httpConfig.setDelayTime(DELAY_TIME);
        httpConfig.setConnectionTimeout(CONNECTION_TIMEOUT);
        that = new HttpConfig();
        that.setUrl(URL);
        that.setDelayTime(DELAY_TIME);
        that.setConnectionTimeout(CONNECTION_TIMEOUT);
    }

    @Test
    public void testGetterSetter() {
        Assertions.assertEquals(URL, httpConfig.getUrl());
        Assertions.assertEquals(DELAY_TIME, httpConfig.getDelayTime());
        Assertions.assertEquals(CONNECTION_TIMEOUT, httpConfig.getConnectionTimeout());
    }

    @Test
    public void testEquals() {
        Assertions.assertEquals(httpConfig, httpConfig);
        Assertions.assertEquals(httpConfig, that);
        Assertions.assertNotEquals(httpConfig, null);
        Assertions.assertNotEquals(httpConfig, new Object());
    }

    @Test
    public void testHashCode() {
        Assertions.assertEquals(Objects.hash(httpConfig.getUrl(), httpConfig.getDelayTime(),
                        httpConfig.getConnectionTimeout(), httpConfig.getReadTimeout(), httpConfig.getWriteTimeout()), httpConfig.hashCode());
    }
}
