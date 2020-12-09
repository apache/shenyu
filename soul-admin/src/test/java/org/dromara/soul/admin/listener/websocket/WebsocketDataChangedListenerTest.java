package org.dromara.soul.admin.listener.websocket;

import com.alibaba.fastjson.JSON;
import io.undertow.Undertow;
import io.undertow.websockets.core.AbstractReceiveListener;
import io.undertow.websockets.core.BufferedTextMessage;
import io.undertow.websockets.core.WebSocketChannel;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.*;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.enums.ReadyState;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.*;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import sun.jvm.hotspot.oops.Metadata;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static io.undertow.Handlers.path;
import static io.undertow.Handlers.websocket;

/**
 * @author : Hyuk
 * @description : WebsocketDataChangedListenerTest
 * @date : 2020/12/6 9:31 下午
 */
@Slf4j
@RunWith(MockitoJUnitRunner.class)
public class WebsocketDataChangedListenerTest {

    private List<PluginData> pluginDataList;
    private List<SelectorData> selectorDataList;
    private List<RuleData> ruleDataList;
    private List<AppAuthData> appAuthDataList;
    private List<MetaData> metaDataList;


    private WebSocketClient client;
    private Undertow server;

    private final AtomicInteger count = new AtomicInteger(0);
    private CountDownLatch countDownLatch = new CountDownLatch(5);

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

    public void startClient() throws InterruptedException, URISyntaxException {
        client = new WebSocketClient(new URI("ws://localhost:8888/websocket")) {
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

        String selectorDataString = "[{\"conditionList\":[{\"operator\":\"match\",\"paramName\":\"/\",\"paramType\":\"uri\",\"paramValue\":\"/http/**\"}],\"continued\":true,\"enabled\":true,\"handle\":\"[{\\\"upstreamHost\\\":\\\"localhost\\\",\\\"protocol\\\":\\\"http://\\\",\\\"upstreamUrl\\\":\\\"127.0.0.1:8187\\\",\\\"weight\\\":\\\"51\\\"},{\\\"upstreamHost\\\":\\\"localhost\\\",\\\"protocol\\\":\\\"http://\\\",\\\"upstreamUrl\\\":\\\"127.0.0.1:8188\\\",\\\"weight\\\":\\\"49\\\"}]\",\"id\":\"1336329408516136960\",\"loged\":true,\"matchMode\":0,\"name\":\"/http\",\"pluginId\":\"5\",\"pluginName\":\"divide\",\"sort\":1,\"type\":1}]";
        selectorDataList = GsonUtils.getInstance().fromList(selectorDataString, SelectorData.class);

        String pluginDataString = "[{\"config\":\"{\\\"model\\\":\\\"black\\\"}\",\"enabled\":true,\"id\":\"2\",\"name\":\"waf\",\"role\":1}]";
        pluginDataList = GsonUtils.getInstance().fromList(pluginDataString, PluginData.class);

        String ruleDataString = "[{\"conditionDataList\":[{\"operator\":\"=\",\"paramName\":\"test\",\"paramType\":\"header\",\"paramValue\":\"a\"}],\"enabled\":true,\"handle\":\"{\\\"permission\\\":\\\"reject\\\",\\\"statusCode\\\":\\\"503\\\"}\",\"id\":\"1336350040008105984\",\"loged\":true,\"matchMode\":1,\"name\":\"test\",\"pluginName\":\"waf\",\"selectorId\":\"1336349806465064960\",\"sort\":1}]";
        ruleDataList = GsonUtils.getInstance().fromList(ruleDataString, RuleData.class);

        String appAuthDataString = "[{\"appKey\":\"D9FD95F496C9495DB5604778A13C3D08\",\"appSecret\":\"02D25048AA1E466F8920E68B08E668DE\",\"enabled\":true,\"paramDataList\":[{\"appName\":\"axiba\",\"appParam\":\"123\"}],\"pathDataList\":[{\"appName\":\"alibaba\",\"enabled\":true,\"path\":\"/1\"}]}]";
        appAuthDataList = GsonUtils.getInstance().fromList(appAuthDataString, AppAuthData.class);

        String metaDataString = "[{\"appName\":\"axiba\",\"enabled\":true,\"methodName\":\"execute\",\"parameterTypes\":\"int\",\"path\":\"/test/execute\",\"rpcExt\":\"1\",\"rpcType\":\"http\",\"serviceName\":\"execute\"}]";
        metaDataList = GsonUtils.getInstance().fromList(metaDataString, MetaData.class);

    }

    public void send(String message) {
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


    public void testOnPluginChanged(){
        WebsocketData<SelectorData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.SELECTOR.name(), DataEventTypeEnum.UPDATE.name(), selectorDataList);
        send(GsonUtils.getInstance().toJson(websocketData));
    }

    public void testOnSelectorChanged() {
        WebsocketData<SelectorData> websocketData =
                new WebsocketData<>(ConfigGroupEnum.SELECTOR.name(), DataEventTypeEnum.UPDATE.name(), selectorDataList);
        send(GsonUtils.getInstance().toJson(websocketData));
    }

    public void testOnRuleChanged() {
        WebsocketData<RuleData> configData =
                new WebsocketData<>(ConfigGroupEnum.RULE.name(), DataEventTypeEnum.UPDATE.name(), ruleDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    public void testOnAppAuthChanged() {
        WebsocketData<AppAuthData> configData =
                new WebsocketData<>(ConfigGroupEnum.APP_AUTH.name(), DataEventTypeEnum.UPDATE.name(), appAuthDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    public void testOnMetaDataChanged() {
        WebsocketData<MetaData> configData =
                new WebsocketData<>(ConfigGroupEnum.META_DATA.name(), DataEventTypeEnum.CREATE.name(), metaDataList);
        send(GsonUtils.getInstance().toJson(configData));
    }

    public void handleMessage(String message) {
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

    private void handleAppAuth(String json) {
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

    private void handleMetaData(String json) {
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

    private void handleSelector(String json) {
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

    private void handlePlugin(String json) {
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

    private void handleRule(String json) {
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
