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

package org.apache.shenyu.plugin.ai.proxy.enhanced.routing;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConversion;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.ShenyuAiRequest;
import org.junit.jupiter.api.Test;
import reactor.core.publisher.Flux;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AiProtocolRouteResolverTest {

    @Test
    void testPrefersNativeRouteWithoutInvokingConverter() {
        AtomicBoolean invoked = new AtomicBoolean();
        AiProtocolConverter converter = converter("client", "native", invoked);
        AiProtocolRouteResolver resolver = resolver(converter);
        ShenyuAiRequest request = request("client");

        AiProtocolRoute route = resolver.resolve(request, Set.of("native", "client"), false);

        assertEquals("client", route.clientProtocol());
        assertEquals("client", route.targetProtocol());
        assertSame(request, route.conversion().request());
        assertFalse(invoked.get());
    }

    @Test
    void testBindsConvertedRequestAndReverseResponseAdapter() {
        AtomicBoolean invoked = new AtomicBoolean();
        AiProtocolRouteResolver resolver = resolver(converter("client", "native", invoked));

        AiProtocolRoute route = resolver.resolve(request("client"), Set.of("native"), true);
        final AiUpstreamResponse response = new AiUpstreamResponse(200, Map.of(), Flux.empty());

        assertEquals("client", route.clientProtocol());
        assertEquals("native", route.targetProtocol());
        assertEquals("native", route.conversion().request().getProtocol());
        assertSame(response, route.conversion().responseAdapter().adapt(response));
        assertTrue(invoked.get());
    }

    @Test
    void testUsesConverterRegistrationOrderForMultipleTargets() {
        AiProtocolConverter first = converter("client", "target-b", new AtomicBoolean());
        AiProtocolConverter second = converter("client", "target-a", new AtomicBoolean());
        AiProtocolRouteResolver resolver = resolver(first, second);

        AiProtocolRoute route = resolver.resolve(request("client"), Set.of("target-a", "target-b"), false);

        assertEquals("target-b", route.targetProtocol());
    }

    @Test
    void testRejectsUnsupportedRoute() {
        AiProtocolRouteResolver resolver = resolver(
                converter("client", "other", new AtomicBoolean()));

        assertThrows(ShenyuException.class,
                () -> resolver.resolve(request("client"), Set.of("native"), false));
    }

    @Test
    void testRejectsConverterReturningWrongTargetProtocol() {
        AiProtocolConverter converter = new AiProtocolConverter() {
            @Override
            public String sourceProtocol() {
                return "client";
            }

            @Override
            public String targetProtocol() {
                return "native";
            }

            @Override
            public AiProtocolConversion convert(final ShenyuAiRequest request, final boolean stream) {
                return AiProtocolConversion.identity(request);
            }
        };

        assertThrows(IllegalArgumentException.class,
                () -> resolver(converter).resolve(request("client"), Set.of("native"), false));
    }

    private AiProtocolRouteResolver resolver(final AiProtocolConverter... converters) {
        return new AiProtocolRouteResolver(new AiProtocolConverterFactory(List.of(converters)));
    }

    private AiProtocolConverter converter(final String source, final String target, final AtomicBoolean invoked) {
        return new AiProtocolConverter() {
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
                invoked.set(true);
                return new AiProtocolConversion(request(target), response -> response);
            }
        };
    }

    private ShenyuAiRequest request(final String protocol) {
        ShenyuAiRequest request = new ShenyuAiRequest();
        request.setProtocol(protocol);
        return request;
    }
}
