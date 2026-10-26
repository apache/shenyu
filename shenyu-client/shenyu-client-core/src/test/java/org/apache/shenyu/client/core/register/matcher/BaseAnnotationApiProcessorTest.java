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
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for {@link BaseAnnotationApiProcessor}.
 */
public final class BaseAnnotationApiProcessorTest {

    @Test
    public void testProcessesMatchingAnnotationsByDefault() throws NoSuchMethodException {
        Method method = AnnotatedService.class.getDeclaredMethod("call");
        ApiBean.ApiDefinition definition = new ApiBean.ApiDefinition(method);
        ApiBean apiBean = new ApiBean("test", "annotatedService", new AnnotatedService(), Collections.singletonList(definition));
        AtomicInteger beanInvocations = new AtomicInteger();
        AtomicInteger definitionInvocations = new AtomicInteger();
        BaseAnnotationApiProcessor<Marker> processor = new BaseAnnotationApiProcessor<Marker>() {
            @Override
            public void process(final ApiBean target, final Marker annotation) {
                beanInvocations.incrementAndGet();
            }

            @Override
            public void process(final ApiBean.ApiDefinition target, final Marker annotation) {
                definitionInvocations.incrementAndGet();
            }

            @Override
            public Class<Marker> matchAnnotation() {
                return Marker.class;
            }
        };

        processor.process(apiBean);

        assertEquals(1, beanInvocations.get());
        assertEquals(1, definitionInvocations.get());
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.TYPE, ElementType.METHOD})
    private @interface Marker {
    }

    @Marker
    private static final class AnnotatedService {

        @Marker
        public void call() {
        }
    }
}
