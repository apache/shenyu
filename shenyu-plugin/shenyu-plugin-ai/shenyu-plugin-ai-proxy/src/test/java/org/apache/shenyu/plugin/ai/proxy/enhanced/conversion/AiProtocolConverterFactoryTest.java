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

package org.apache.shenyu.plugin.ai.proxy.enhanced.conversion;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiProtocolConverterFactoryTest {

    @Test
    void testFindsConverterByDirectionalRoute() {
        AiProtocolConverter converter = new TestConverter("openai-responses", "openai-chat");
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of(converter));

        assertSame(converter, factory.getConverter("openai-responses", "openai-chat"));
        assertThrows(ShenyuException.class,
                () -> factory.getConverter("openai-chat", "openai-responses"));
    }

    @Test
    void testRejectsDuplicateRoute() {
        assertThrows(IllegalArgumentException.class, () -> new AiProtocolConverterFactory(List.of(
                new TestConverter("source", "target"),
                new TestConverter("source", "target"))));
    }

    @Test
    void testRejectsUnsupportedRoute() {
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of());

        assertThrows(ShenyuException.class, () -> factory.getConverter("source", "target"));
    }

    @Test
    void testSupportsIdentityWithoutRegistration() {
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of());

        assertTrue(factory.supports("openai-chat", "openai-chat"));
        assertFalse(factory.supports("openai-responses", "openai-chat"));
    }

    @Test
    void testCreatesIdentityConverterWithoutRegistration() {
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of());
        ShenyuAiRequest request = new ShenyuAiRequest();
        request.setProtocol("openai-chat");

        AiProtocolConverter converter = factory.getConverter("openai-chat", "openai-chat");
        AiProtocolConversion conversion = converter.convert(request, false);

        assertTrue(converter instanceof IdentityAiProtocolConverter);
        assertSame(request, conversion.request());
    }

    @Test
    void testPrefersNativeProviderProtocolOverConversion() {
        AiProtocolConverter converter = new TestConverter("openai-responses", "openai-chat");
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of(converter));

        AiProtocolConverter resolved = factory.resolveConverter(
                "openai-responses", Set.of("openai-chat", "openai-responses"));

        assertTrue(resolved instanceof IdentityAiProtocolConverter);
    }

    @Test
    void testSelectsRegisteredRouteToAnyProviderProtocol() {
        AiProtocolConverter first = new TestConverter("source", "target-b");
        AiProtocolConverter second = new TestConverter("source", "target-a");
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of(first, second));

        AiProtocolConverter resolved = factory.resolveConverter(
                "source", Set.of("target-a", "target-b"));

        assertSame(first, resolved);
    }

    @Test
    void testRejectsSourceWithoutRouteToProviderProtocols() {
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of(
                new TestConverter("source", "other")));

        assertThrows(ShenyuException.class,
                () -> factory.resolveConverter("source", Set.of("target-a", "target-b")));
    }

    @Test
    void testRejectsInvalidProtocolIdentifiers() {
        AiProtocolConverterFactory factory = new AiProtocolConverterFactory(List.of());

        assertFalse(factory.supports(null, null));
        assertFalse(factory.supports("", ""));
        assertThrows(NullPointerException.class, () -> factory.getConverter(null, null));
        assertThrows(IllegalArgumentException.class, () -> factory.getConverter("", ""));
    }

    private static final class TestConverter implements AiProtocolConverter {

        private final String source;

        private final String target;

        private TestConverter(final String source, final String target) {
            this.source = source;
            this.target = target;
        }

        @Override
        public String sourceProtocol() {
            return source;
        }

        @Override
        public String targetProtocol() {
            return target;
        }

        @Override
        public AiProtocolConversion convert(final ShenyuAiRequest request, final boolean stream) {
            return AiProtocolConversion.identity(request);
        }
    }
}
