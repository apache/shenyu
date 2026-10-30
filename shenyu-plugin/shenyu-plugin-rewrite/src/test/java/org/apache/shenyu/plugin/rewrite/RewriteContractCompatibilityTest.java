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

package org.apache.shenyu.plugin.rewrite;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.rewrite.handler.RewritePluginDataHandler;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

class RewriteContractCompatibilityTest {

    @ParameterizedTest(name = "{0}")
    @MethodSource("fixtures")
    void sharedFixturesCoverPublicationAndRuntime(final String name, final String action, final String source,
                                                  final String path, final String rewritten, final String metadata, final String contextPath) {
        final RewritePluginDataHandler handler = new RewritePluginDataHandler();
        RuleData rule = new RuleData();
        rule.setSelectorId("contract-selector");
        rule.setId(name);
        rule.setHandle("{\"regex\":\"/old\",\"replace\":\"/new\"}");
        handler.handlerRule(rule);
        String key = CacheKeyUtils.INST.getKey(rule);
        final RewriteHandle previous = RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key);
        rule.setHandle(source);
        try {
            if ("THROW_RETAIN".equals(action)) {
                assertThrows(RuntimeException.class, () -> handler.handlerRule(rule));
            } else {
                handler.handlerRule(rule);
            }
            RewriteHandle current = RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key);
            if ("RETAIN".equals(action) || "THROW_RETAIN".equals(action)) {
                assertSame(previous, current);
            } else if ("REPLACE".equals(action)) {
                assertNotNull(current);
                assertNotSame(previous, current);
            } else {
                assertNull(current);
            }
            MockServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get(path).build());
            ShenyuContext context = new ShenyuContext();
            context.setRpcType("http");
            exchange.getAttributes().put(Constants.CONTEXT, context);
            String prefix = "@missing".equals(contextPath) ? "" : contextPath;
            context.setRealUrl(prefix.isEmpty() ? path : path.substring(prefix.length()));
            if (!prefix.isEmpty()) {
                exchange.getAttributes().put(Constants.CONTEXT_PATH, prefix);
            }
            if ("enabled".equals(metadata) || "disabled".equals(metadata)) {
                String metadataPath = prefix + rewritten;
                MetaData data = MetaData.builder().id("contract-meta").path(metadataPath).rpcType("dubbo").enabled("enabled".equals(metadata)).build();
                MetaDataCache.getInstance().initCache(metadataPath, data, metadataPath);
            }
            AtomicInteger continued = new AtomicInteger();
            StepVerifier.create(new RewritePlugin().doExecute(exchange, next -> {
                continued.incrementAndGet();
                return Mono.empty();
            }, new SelectorData(), rule)).verifyComplete();
            assertEquals(1, continued.get());
            assertEquals("@none".equals(rewritten) ? null : rewritten, exchange.getAttribute(Constants.REWRITE_URI));
            assertEquals("enabled".equals(metadata) ? "dubbo" : "http", context.getRpcType());
            handler.removeRule(rule);
            handler.removeRule(rule);
            assertNull(RewritePluginDataHandler.CACHED_HANDLE.get().obtainHandle(key));
        } finally {
            handler.removeRule(rule);
            MetaDataCache.getInstance().clean();
        }
    }

    private static java.util.stream.Stream<org.junit.jupiter.params.provider.Arguments> fixtures() throws java.io.IOException {
        try (var input = java.util.Objects.requireNonNull(RewriteContractCompatibilityTest.class.getResourceAsStream(
                "/META-INF/shenyu/config-contract-fixtures/rewrite-rule-v1.tsv"));
             var reader = new java.io.BufferedReader(new java.io.InputStreamReader(input, java.nio.charset.StandardCharsets.UTF_8))) {
            return reader.lines().filter(line -> !line.isBlank() && !line.startsWith("#")).map(line -> {
                String[] fields = line.split("\\t", -1);
                String source = "@missing".equals(fields[3]) ? null : "@blank".equals(fields[3]) ? "" : fields[3];
                return org.junit.jupiter.params.provider.Arguments.of(fields[0], fields[2], source, fields[4], fields[5], fields[6], fields[7]);
            }).toList().stream();
        }
    }
}
