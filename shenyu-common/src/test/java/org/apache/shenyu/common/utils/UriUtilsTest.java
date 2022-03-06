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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.Test;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for UriUtils.
 */
public final class UriUtilsTest {

    @Test
    void createUri() {
        URI uri = UriUtils.createUri("https://example.com");
        assertEquals("https://example.com", uri.toString());

        uri = UriUtils.createUri(null);
        assertNull(uri);

        uri = UriUtils.createUri("");
        assertNull(uri);
    }

    @Test
    void repairData() {
        String ret = UriUtils.repairData("http");
        assertEquals("/http", ret);

        ret = UriUtils.repairData("/http");
        assertEquals("/http", ret);
    }

    @Test
    void removePrefix() {
        String ret = UriUtils.removePrefix("http");
        assertEquals("http", ret);

        ret = UriUtils.removePrefix("/http");
        assertEquals("http", ret);
    }

    @Test
    void getPathWithParams() {
        URI uri = UriUtils.createUri("https://example.com");
        assertNotNull(uri);
        String ret = UriUtils.getPathWithParams(uri);
        assertEquals("", ret);

        uri = UriUtils.createUri("https://example.com/path");
        assertNotNull(uri);
        ret = UriUtils.getPathWithParams(uri);
        assertEquals("/path", ret);

        uri = UriUtils.createUri("https://example.com/path?key=val");
        assertNotNull(uri);
        ret = UriUtils.getPathWithParams(uri);
        assertEquals("/path?key=val", ret);

        uri = UriUtils.createUri("/path?key=val");
        assertNotNull(uri);
        ret = UriUtils.getPathWithParams(uri);
        assertEquals("/path?key=val", ret);
    }
}
