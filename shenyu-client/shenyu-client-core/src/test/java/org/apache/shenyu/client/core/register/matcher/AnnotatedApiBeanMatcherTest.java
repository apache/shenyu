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
import org.junit.jupiter.api.Test;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class AnnotatedApiBeanMatcherTest {

    @Test
    public void testMatchAnnotatedBean() {
        ApiBean apiBean = new ApiBean("test", "annotatedBean", new AnnotatedService());
        AnnotatedApiBeanMatcher matcher = new AnnotatedApiBeanMatcher(BeanMarker.class);
        assertTrue(matcher.match(apiBean));
    }

    @Test
    public void testMatchPlainBean() {
        ApiBean apiBean = new ApiBean("test", "plainBean", new PlainService());
        AnnotatedApiBeanMatcher matcher = new AnnotatedApiBeanMatcher(BeanMarker.class);
        assertFalse(matcher.match(apiBean));
    }

    @Test
    public void testMatchDoesNotConsiderInheritedAnnotations() {
        ApiBean apiBean = new ApiBean("test", "inheritedBean", new InheritedService());
        AnnotatedApiBeanMatcher matcher = new AnnotatedApiBeanMatcher(BeanMarker.class);
        assertFalse(matcher.match(apiBean),
                "isAnnotationDeclaredLocally should not match annotations inherited from a superclass");
    }

    @Test
    public void testMatchOtherAnnotation() {
        ApiBean apiBean = new ApiBean("test", "annotatedBean", new AnnotatedService());
        AnnotatedApiBeanMatcher matcher = new AnnotatedApiBeanMatcher(Deprecated.class);
        assertFalse(matcher.match(apiBean));
    }

    @Test
    public void testMatchWithEmptyApiDefinitions() {
        ApiBean apiBean = new ApiBean("test", "annotatedBean", new AnnotatedService(), Collections.emptyList());
        AnnotatedApiBeanMatcher matcher = new AnnotatedApiBeanMatcher(BeanMarker.class);
        assertTrue(matcher.match(apiBean));
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE)
    @interface BeanMarker {
    }

    @BeanMarker
    static class AnnotatedService {
    }

    static class PlainService {
    }

    static class InheritedService extends AnnotatedService {
    }
}
