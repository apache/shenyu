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

package org.apache.shenyu.integrated.test.websocket;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.WebSocketUpstream;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class WebsocketPluginTest extends AbstractPluginDataInit {

    private static final Logger LOG = LoggerFactory.getLogger(WebsocketPluginTest.class);

    private static final String WEBSOCKET_URI = "ws://localhost:9195/websocket";

    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.WEB_SOCKET.getName(), "{\"multiSelectorHandle\":\"1\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.WEB_SOCKET.getName(),
                buildSelectorHandler(), buildSelectorConditionList(), buildRuleLocalDataList());
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void testWebsocket() throws URISyntaxException, InterruptedException {
        final String sendMessage = "shenyu-test";
        ArrayBlockingQueue<String> blockingQueue = new ArrayBlockingQueue<>(1);
        WebSocketClient webSocketClient = new WebSocketClient(new URI(WEBSOCKET_URI)) {
            @Override
            public void onOpen(final ServerHandshake serverHandshake) {
            }

            @Override
            public void onMessage(final String s) {
                LOG.info("websocket client received message : {}", s);
                blockingQueue.add(s);
            }

            @Override
            public void onClose(final int i, final String s, final boolean b) {
            }

            @Override
            public void onError(final Exception e) {
            }
        };
        webSocketClient.connectBlocking();
        webSocketClient.send(sendMessage);
        String receivedMessage = blockingQueue.poll(10, TimeUnit.SECONDS);
        assertThat(receivedMessage, is("result apache shenyu : -> " + sendMessage));
    }

    private static String buildSelectorHandler() {
        WebSocketUpstream upstream = WebSocketUpstream.builder()
                .upstreamUrl("shenyu-examples-websocket:8848")
                .protocol("ws://")
                .weight(50)
                .timestamp(0)
                .warmup(0)
                .status(true)
                .build();
        return JsonUtils.toJson(Collections.singleton(upstream));
    }

    private static List<ConditionData> buildSelectorConditionList() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/websocket");
        return Collections.singletonList(conditionData);
    }

    private static List<RuleLocalData> buildRuleLocalDataList() {
        final RuleLocalData ruleLocalData = new RuleLocalData();

        WebSocketRuleHandle ruleHandle = new WebSocketRuleHandle();
        ruleHandle.setLoadBalance("roundRobin");
        ruleHandle.setRetry(1);
        ruleHandle.setTimeout(3000);
        ruleLocalData.setRuleHandler(JsonUtils.toJson(ruleHandle));

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.EQ.getAlias());
        conditionData.setParamValue("/websocket");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));

        return Collections.singletonList(ruleLocalData);
    }
}
