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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.AiProtocolConverterFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.conversion.ConnectResponsesToChatConverter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.routing.AiProtocolRouteResolver;

import java.util.List;

/**
 * Test utilities for AI proxy fixtures.
 */
public final class AiProxyTestUtils {

    private AiProxyTestUtils() {
    }

    /**
     * Creates the protocol route resolver used by the default Spring configuration.
     *
     * @return protocol route resolver
     */
    public static AiProtocolRouteResolver protocolRouteResolver() {
        AiProtocolConverterFactory converterFactory =
                new AiProtocolConverterFactory(List.of(new ConnectResponsesToChatConverter()));
        return new AiProtocolRouteResolver(converterFactory);
    }
}
