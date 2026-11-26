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

package org.apache.shenyu.client.core.utils;

import org.junit.jupiter.api.Test;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class RequestMethodUtilsTest {

    @Test
    void testComposedMappingUsesDeclaredHttpMethod() throws NoSuchMethodException {
        assertEquals(List.of("POST"), RequestMethodUtils.getRequestMethodTypes(getClass().getDeclaredMethod("composed")));
    }

    @Test
    void testStandardAndExplicitMappingsKeepTheirMethods() throws NoSuchMethodException {
        assertEquals(List.of("GET"), RequestMethodUtils.getRequestMethodTypes(getClass().getDeclaredMethod("standard")));
        assertEquals(List.of("PUT", "PATCH"), RequestMethodUtils.getRequestMethodTypes(getClass().getDeclaredMethod("explicit")));
    }

    @Test
    void testUnconstrainedAndUnmappedMethodsKeepTheirDefaults() throws NoSuchMethodException {
        assertEquals(List.of("GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS", "HEAD"),
                RequestMethodUtils.getRequestMethodTypes(getClass().getDeclaredMethod("unconstrained")));
        assertEquals(List.of(), RequestMethodUtils.getRequestMethodTypes(getClass().getDeclaredMethod("unmapped")));
    }

    @RequestMapping(path = "/write", method = RequestMethod.POST)
    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    @interface WriteMapping {
    }

    @WriteMapping
    void composed() {
    }

    @GetMapping
    void standard() {
    }

    @RequestMapping(method = {RequestMethod.PUT, RequestMethod.PATCH})
    void explicit() {
    }

    @RequestMapping
    void unconstrained() {
    }

    void unmapped() {
    }
}
