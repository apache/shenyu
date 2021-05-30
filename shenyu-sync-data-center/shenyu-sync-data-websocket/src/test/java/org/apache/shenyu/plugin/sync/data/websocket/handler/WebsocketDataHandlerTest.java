package org.apache.shenyu.plugin.sync.data.websocket.handler;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.util.LinkedList;
import java.util.List;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;


public class WebsocketDataHandlerTest {
    PluginDataSubscriber pluginDataSubscriber;
    WebsocketDataHandler websocketDataHandler;

    @Before
    public void testWebsocketDataHandler() {
        pluginDataSubscriber = mock(PluginDataSubscriber.class);

        List<AuthDataSubscriber> authDataSubscribers = new LinkedList<>();
        List<MetaDataSubscriber> metaDataSubscribers = new LinkedList<>();

        websocketDataHandler = new WebsocketDataHandler(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
    }

    @Test
    public void testPluginRefreshExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.REFRESH.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        Mockito.verify(pluginDataSubscriber).refreshPluginDataSelf(pluginDataList);
    }

    @Test
    public void testPluginMyselfExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.MYSELF.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        Mockito.verify(pluginDataSubscriber).refreshPluginDataSelf(pluginDataList);
    }

    @Test
    public void testPluginUpdateExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.UPDATE.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        pluginDataList.forEach(verify(pluginDataSubscriber)::onSubscribe);
    }

    @Test
    public void testPluginCreateExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.CREATE.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        pluginDataList.forEach(verify(pluginDataSubscriber)::onSubscribe);
    }

    @Test
    public void testPluginDeleteExecutor() {
        String json = getJson();
        websocketDataHandler.executor(ConfigGroupEnum.PLUGIN, json, DataEventTypeEnum.DELETE.name());
        List<PluginData> pluginDataList = new PluginDataHandler(pluginDataSubscriber).convert(json);
        pluginDataList.forEach(verify(pluginDataSubscriber)::unSubscribe);
    }

    private String getJson() {
        PluginData pluginData = new PluginData();
        pluginData.setId("1397952341475799040");
        pluginData.setName("plugin_test");
        pluginData.setConfig("config_test");
        pluginData.setEnabled(true);
        pluginData.setRole(1);
        LinkedList<PluginData> list = new LinkedList<>();
        list.add(pluginData);
        return GsonUtils.getGson().toJson(list);
    }
}