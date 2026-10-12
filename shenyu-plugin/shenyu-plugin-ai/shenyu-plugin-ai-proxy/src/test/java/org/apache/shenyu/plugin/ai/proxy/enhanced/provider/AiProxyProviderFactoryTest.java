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

package org.apache.shenyu.plugin.ai.proxy.enhanced.provider;

import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

class AiProxyProviderFactoryTest {

    @Test
    void testProviderNameIsNormalized() {
        final OpenAi provider = new OpenAi();
        final AiProxyProviderFactory factory = new AiProxyProviderFactory(List.of(provider));

        assertSame(provider, factory.getProvider(" Open-AI "));
        assertSame(provider, factory.getProvider("OPEN_AI"));
    }

    @Test
    void testRejectsDuplicateAndUnsupportedProviders() {
        assertThrows(IllegalArgumentException.class,
                () -> new AiProxyProviderFactory(List.of(new OpenAi(), new OpenAi())));

        final AiProxyProviderFactory factory = new AiProxyProviderFactory(List.of(new OpenAi()));
        assertThrows(ShenyuException.class, () -> factory.getProvider("unknown"));
        assertThrows(ShenyuException.class, () -> factory.getProvider(null));
    }
}
