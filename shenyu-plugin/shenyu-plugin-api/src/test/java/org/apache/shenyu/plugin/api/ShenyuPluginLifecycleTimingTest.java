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

package org.apache.shenyu.plugin.api;

import org.apache.shenyu.common.constant.Constants;
import org.junit.jupiter.api.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Behavior pin for the plugin timing lifecycle of
 * {@link ShenyuPlugin#before}/{@link ShenyuPlugin#after} (#8437): the
 * attribute key, the debug log and the lifecycle are unchanged — in
 * particular, {@code after} without a {@code before} keeps failing loudly.
 */
public class ShenyuPluginLifecycleTimingTest {

    private final ServerWebExchange exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/x").build());

    private final ShenyuPlugin plugin = new ShenyuPlugin() {
        @Override
        public Mono<Void> execute(final ServerWebExchange serverWebExchange, final ShenyuPluginChain chain) {
            return chain.execute(serverWebExchange);
        }

        @Override
        public int getOrder() {
            return 0;
        }

        @Override
        public String named() {
            return "timing-test";
        }
    };

    @Test
    public void beforeRecordsAndAfterRemovesTheTimingAttribute() throws InterruptedException {
        plugin.before(exchange);
        String key = Constants.PLUGIN_START_TIME + plugin.named();
        assertNotNull(exchange.getAttributes().get(key));
        Thread.sleep(5);
        plugin.after(exchange);
        assertNull(exchange.getAttributes().get(key));
    }

    @Test
    public void afterWithoutBeforeKeepsFailingLoudly() {
        assertThrows(NullPointerException.class, () -> plugin.after(exchange));
    }
}
