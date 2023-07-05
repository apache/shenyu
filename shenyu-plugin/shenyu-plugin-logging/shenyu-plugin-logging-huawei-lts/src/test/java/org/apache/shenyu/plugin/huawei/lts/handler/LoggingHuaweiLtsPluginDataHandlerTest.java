package org.apache.shenyu.plugin.huawei.lts.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.huawei.lts.client.HuaweiLtsLogCollectClient;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

public class LoggingHuaweiLtsPluginDataHandlerTest {

    private LoggingHuaweiLtsPluginDataHandler loggingHuaweiLtsPluginDataHandler;

    private final PluginData pluginData=new PluginData();

    @BeforeEach
    private void setUp(){
        this.loggingHuaweiLtsPluginDataHandler=new LoggingHuaweiLtsPluginDataHandler();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\n" +
                "    \"projectId\": \"3d696d796b9647f982b5ec787b2ca249\",\n" +
                "    \"logGroupId\": \"8285fbe3-9c73-4deb-a8d0-96ec06c4af3a\",\n" +
                "    \"logStreamId\": \"8b4fe176-4a7e-4bd5-8bb7-17d0ec6730b2\",\n" +
                "    \"accessKeyId\": \"FNUGUUNZL5CG6ZJDAFXX\",\n" +
                "    \"accessKeySecret\": \"g7WwPGAmCoa5Uz62lsxzHDZeSyYVNNePSjkd2jI8\",\n" +
                "    \"regionName\": \"cn-north-4\"\n" +
                "}");
    }

    @Test
    public void testHandlerPlugin() throws NoSuchFieldException, IllegalAccessException {
        loggingHuaweiLtsPluginDataHandler.handlerPlugin(pluginData);
        Field field = loggingHuaweiLtsPluginDataHandler.getClass().getDeclaredField("HUAWEI_LTS_LOG_COLLECT_CLIENT");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(loggingHuaweiLtsPluginDataHandler).getClass(), HuaweiLtsLogCollectClient.class);
        pluginData.setEnabled(true);
        loggingHuaweiLtsPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(loggingHuaweiLtsPluginDataHandler.pluginNamed(), PluginEnum.LOGGING_HUAWEI_LTS.getName());
    }

    @Test
    public void testGetHuaweiLtsLogCollectClient() {
        Assertions.assertEquals(LoggingHuaweiLtsPluginDataHandler.getHuaweiLtsLogCollectClient().getClass(), HuaweiLtsLogCollectClient.class);
    }
}
