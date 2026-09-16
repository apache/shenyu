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

package org.apache.shenyu.client.core.register.matcher;

import org.apache.shenyu.client.core.register.ApiBean;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class AnnotatedApiDefinitionMatcherTest {

    private ApiBean apiBean;

    @BeforeEach
    public void setUp() {
        apiBean = new ApiBean("test", "serviceBean", new Service());
    }

    private ApiBean.ApiDefinition apiDefinitionOf(final String methodName) throws NoSuchMethodException {
        Method method = Service.class.getDeclaredMethod(methodName);
        return new ApiBean.ApiDefinition(method);
    }

    @Test
    public void testMatchAnnotatedMethod() throws NoSuchMethodException {
        AnnotatedApiDefinitionMatcher matcher = new AnnotatedApiDefinitionMatcher(ApiMarker.class);
        assertTrue(matcher.match(apiDefinitionOf("annotatedMethod")));
    }

    @Test
    public void testMatchPlainMethod() throws NoSuchMethodException {
        AnnotatedApiDefinitionMatcher matcher = new AnnotatedApiDefinitionMatcher(ApiMarker.class);
        assertFalse(matcher.match(apiDefinitionOf("plainMethod")));
    }

    @Test
    public void testMatchOtherAnnotation() throws NoSuchMethodException {
        AnnotatedApiDefinitionMatcher matcher = new AnnotatedApiDefinitionMatcher(Deprecated.class);
        assertFalse(matcher.match(apiDefinitionOf("annotatedMethod")));
    }

    @Test
    public void testMatchApiDefinitionFromApiBean() throws NoSuchMethodException {
        apiBean.addApiDefinition(Service.class.getDeclaredMethod("annotatedMethod"), "/annotated");
        apiBean.addApiDefinition(Service.class.getDeclaredMethod("plainMethod"), "/plain");

        AnnotatedApiDefinitionMatcher matcher = new AnnotatedApiDefinitionMatcher(ApiMarker.class);

        assertTrue(matcher.match(apiBean.getApiDefinitions().get(0)));
        assertFalse(matcher.match(apiBean.getApiDefinitions().get(1)));
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    @interface ApiMarker {
    }

    static class Service {

        @ApiMarker
        public void annotatedMethod() {
        }

        public void plainMethod() {
        }
    }
}
