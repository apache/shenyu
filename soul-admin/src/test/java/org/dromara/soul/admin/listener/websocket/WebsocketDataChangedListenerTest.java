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

package org.dromara.soul.admin.listener.websocket;

import io.undertow.Undertow;
import io.undertow.websockets.core.AbstractReceiveListener;
import io.undertow.websockets.core.BufferedTextMessage;
import io.undertow.websockets.core.WebSocketChannel;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.AuthPathData;
import org.dromara.soul.common.dto.AuthParamData;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.WebsocketData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.enums.ReadyState;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.Assert;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static io.undertow.Handlers.path;
import static io.undertow.Handlers.websocket;

/**
 * Data Change WebSocketListener Test.
 *
 * @author : Hyuk
 */
@Slf4j
@RunWith(MockitoJUnitRunner.class)
public final class WebsocketDataChangedListenerTest {

    private final List<PluginData> pluginDataList = new ArrayList<>();

    private final List<SelectorData> selectorDataList = new ArrayList<>();

    private final List<RuleData> ruleDataList = new ArrayList<>();

    private final List<AppAuthData> appAuthDataList = new ArrayList<>();

    private final List<MetaData> metaDataList = new ArrayList<>();

    private WebSocketClient client;

    private Undertow server;

    private final AtomicInteger count = new AtomicInteger(0);

    private CountDownLatch countDownLatch = new CountDownLatch(5);

    /**
     * start websocket server.
     */
    public void startServer() {
        server = Undertow.builder()
                .addHttpListener(8888, "localhost")
                .setHandler(path()
                        .addPrefixPath("/websocket", websocket((exchange, channel) -> {
                            channel.getReceiveSetter().set(new AbstractReceiveListener() {
                                @Override
                                protected void onFullTextMessage(final WebSocketChannel channel, final BufferedTextMessage message) {
                                    handleMessage(message.getData());
                                }
                            });
                            channel.resumeReceives();
                        })))
                .build();
        server.start();
    }

    /**
     * start websocket client.
     *
     * @throws URISyntaxException URISyntaxException
     * @throws InterruptedException InterruptedException
     */
    public void startClient() throws URISyntaxException, InterruptedException {
        InetSocketAddress inetSocketAddress = (InetSocketAddress) server.getListenerInfo().get(0).getAddress();
        StringBuilder sb = new StringBuilder();
        sb.append("ws://").append(inetSocketAddress.getHostName()).append(":").append(inetSocketAddress.getPort())
                .append("/websocket");

        client = new WebSocketClient(new URI(sb.toString())) {
            @Override
            public void onOpen(final ServerHandshake serverHandshake) {
                log.info("Open connection");
            }

            @Override
            public void onMessage(final String s) {
                log.info("message : {}", s);
            }

            @Override
            public void onClose(final int i, final String s, final boolean b) {
            }

            @Override
            public void onError(final Exception e) {
                log.error("", e);
            }
        };
        client.connect();
        while (!client.getReadyState().equals(ReadyState.OPEN)) {
            log.debug("connecting...");
            TimeUnit.SECONDS.sleep(1);
        }
    }

    @After
    public void destroy() {
        client.close();
        server.stop();
    }

    @Before
    public void before() throws InterruptedException, URISyntaxException {
        startServer();
        startClient();
        countDownLatch = new CountDownLatch(5);
        initSelectorDataList();
        initPluginDataList();
        initRuleDataList();
        initAppAuthDataList();
        initMetaDataList();
    }

    private void initMetaDataList() {
        MetaData metaData = new MetaData();
        metaData.setAppName("axiba");
        metaData.setEnabled(true);
        metaData.setMethodName("execute");
        metaData.setParameterTypes("int");
        metaData.setPath("/test/execute");
        metaData.setRpcExt("{}");
        metaData.setRpcType("http");
        metaData.setServiceName("execute");
        metaDataList.add(metaData);
    }

    private void initAppAuthDataList() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604778A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        appAuthData.setParamDataList(buildAuthParamDataList("axiba", "123"));
        appAuthData.setPathDataList(buildAuthPathDataList("alibaba", "/1"));
        appAuthDataList.add(appAuthData);
    }

    private void initPluginDataList() {
        PluginData pluginData = new PluginData();
        pluginData.setConfig("{\\\"model\\\":\\\"black\\\"}");
        pluginData.setEnabled(true);
        pluginData.setId("2");
        pluginData.setName("waf");
        pluginData.setRole(1);
        pluginDataList.add(pluginData);
    }

    private void initRuleDataList() {
        RuleData ruleData = new RuleData();
        ruleData.setEnabled(true);
        ruleData.setHandle("{\\\"permission\\\":\\\"reject\\\",\\\"statusCode\\\":\\\"503\\\"}");
        ruleData.setId("1336350040008105984");
        ruleData.setLoged(true);
        ruleData.setMatchMode(1);
        ruleData.setName("test");
        ruleData.setPluginName("waf");
        ruleData.setSelectorId("1336349806465064960");
        ruleData.setSort(1);
        ruleData.setConditionDataList(buildConditionDataList("=", "test", "header", "a"));
        ruleDataList.add(ruleData);
    }

    private void initSelectorDataList() {
        SelectorData selectorData = new SelectorData();
        selectorData.setContinued(true);
        selectorData.setEnabled(true);
        selectorData.setHandle("[{\\\"upstreamHost\\\":\\\"localhost\\\",\\\"protocol\\\":\\\"http://\\\",\\\"upstreamUrl\\\":\\\"127.0.0.1:8187\\\","
                + "\\\"weight\\\":\\\"51\\\"},{\\\"upstreamHost\\\":\\\"localhost\\\",\\\"protocol\\\":\\\"http://\\\",\\\"upstreamUrl\\\":\\\"127.0.0.1:8188\\\",\\\"weight\\\":\\\"49\\\"}]");
        selectorData.setId("1336329408516136960");
        selectorData.setLoged(true);
        selectorData.setMatchMode(0);
        selectorData.setName("/http");
        selectorData.setPluginId("5");
        selectorData.setPluginName("divide");
        selectorData.setSort(1);
        selectorData.setType(1);
        selectorData.setConditionList(buildConditionDataList("match", "/", "uri", "/http/**"));
        selectorDataList.add(selectorData);
    }

    private List<ConditionData> buildConditionDataList(final String operator, final String paramName,
                                                       final String paramType, final String paramValue) {
        ConditionData conditionData = new ConditionData();
        conditionData.setOperator(operator);
        conditionData.setParamName(paramName);
        conditionData.setParamType(paramType);
        conditionData.setParamValue(paramValue);
        List<ConditionData> conditionList = new ArrayList<>();
        conditionList.add(conditionData);
        return conditionList;
    }

    private List<AuthParamData> buildAuthParamDataList(final String appName, final String appParam) {
        AuthParamData authParamData = new AuthParamData();
        authParamData.setAppName(appName);
        authParamData.setAppParam(appParam);

        List<AuthParamData> authParamDataList = new ArrayList<>();
        authParamDataList.add(authParamData);
        return authParamDataList;
    }

    private List<AuthPathData> buildAuthPathDataList(final String appName, final String path) {
        AuthPathData authPathData = new AuthPathData();
        authPathData.setAppName(appName);
        authPathData.setEnabled(true);
        authPathData.setPath(path);

        List<AuthPathData> authPathDataList = new ArrayList<>();
        authPathDataList.add(authPathData);
        return authPathDataList;
    }

    /**
     * Send data change message.
     *
     * @param message message
     */
    public void send(final String message) {
        client.send(message);
    }

    @Test
    public void testChanged() throws InterruptedException {
        testOnPluginChanged();
        testOnAppAuthChanged();
        testOnMetaDataChanged();
        testOnRuleChanged();
        testOnSelectorChanged();

        countDownLatch.await(10, TimeUnit.SECONDS);
        Assert.assertEquals(5, count.get());
    }


    /**
     * test PluginData.
     */
    public void testOnPluginChanged() {
        WebsocketData<SelectorData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.SELECTOR.name(), DataEventTypeEnum.UPDATE.name(), selectorDataList);
        send(GsonUtils.getInstance().toJson(websocketData));
    }

    /**
     * test SelectorData.
     */
    public void testOnSelectorChanged() {
        WebsocketData<SelectorData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.SELECTOR.name(), DataEventTypeEnum.UPDATE.name(), selectorDataList);
        send(GsonUtils.getInstance().toJson(websocketData));
    }

    /**
     * test RuleData.
     */
    public void testOnRuleChanged() {
        WebsocketData<RuleData> configData =
                new WebsocketData<>(ConfigGroupEnum.RULE.name(), DataEventTypeEnum.UPDATE.name(), ruleDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    /**
     * test AppAuthData.
     */
    public void testOnAppAuthChanged() {
        WebsocketData<AppAuthData> configData =
                new WebsocketData<>(ConfigGroupEnum.APP_AUTH.name(), DataEventTypeEnum.UPDATE.name(), appAuthDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    /**
     * test MetaData.
     */
    public void testOnMetaDataChanged() {
        WebsocketData<MetaData> configData =
                new WebsocketData<>(ConfigGroupEnum.META_DATA.name(), DataEventTypeEnum.CREATE.name(), metaDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    /**
     * Handle data change message.
     *
     * @param message message
     */
    public void handleMessage(final String message) {
        Assert.assertNotNull(message);
        WebsocketData websocketData = GsonUtils.getInstance().fromJson(message, WebsocketData.class);
        ConfigGroupEnum groupEnum = ConfigGroupEnum.acquireByName(websocketData.getGroupType());
        Assert.assertNotNull(groupEnum);

        String eventType = websocketData.getEventType();
        DataEventTypeEnum eventTypeEnum = DataEventTypeEnum.acquireByName(eventType);
        Assert.assertNotNull(eventTypeEnum);

        String json = GsonUtils.getInstance().toJson(websocketData.getData());
        Assert.assertNotNull(json);

        switch (groupEnum) {
            case RULE:
                handleRule(json);
                break;
            case PLUGIN:
                handlePlugin(json);
                break;
            case SELECTOR:
                handleSelector(json);
                break;
            case APP_AUTH:
                handleAppAuth(json);
                break;
            case META_DATA:
                handleMetaData(json);
                break;
            default:
                throw new RuntimeException("unknown groupEnum");
        }
    }

    private void handleAppAuth(final String json) {
        try {
            List<AppAuthData> appAuthData = GsonUtils.getInstance().fromList(json, AppAuthData.class);
            Assert.assertEquals(appAuthData, appAuthDataList);
            count.incrementAndGet();
        } catch (Exception e) {
            throw e;
        } finally {
            countDownLatch.countDown();
        }
    }

    private void handleMetaData(final String json) {
        try {
            List<MetaData> metaData = GsonUtils.getInstance().fromList(json, MetaData.class);
            Assert.assertEquals(metaData, metaDataList);
            count.incrementAndGet();
        } catch (Exception e) {
            throw e;
        } finally {
            countDownLatch.countDown();
        }
    }

    private void handleSelector(final String json) {
        try {
            List<SelectorData> selectorData = GsonUtils.getInstance().fromList(json, SelectorData.class);
            Assert.assertEquals(selectorData, selectorDataList);
            count.incrementAndGet();
        } catch (Exception e) {
            throw e;
        } finally {
            countDownLatch.countDown();
        }
    }

    private void handlePlugin(final String json) {
        try {
            List<PluginData> pluginData = GsonUtils.getInstance().fromList(json, PluginData.class);
            Assert.assertEquals(pluginData, pluginDataList);
            count.incrementAndGet();
        } catch (Exception e) {
            throw e;
        } finally {
            countDownLatch.countDown();
        }
    }

    private void handleRule(final String json) {
        try {
            List<RuleData> ruleData = GsonUtils.getInstance().fromList(json, RuleData.class);
            Assert.assertEquals(ruleData, ruleDataList);
            count.incrementAndGet();
        } catch (Exception e) {
            throw e;
        } finally {
            countDownLatch.countDown();
        }
    }
}
