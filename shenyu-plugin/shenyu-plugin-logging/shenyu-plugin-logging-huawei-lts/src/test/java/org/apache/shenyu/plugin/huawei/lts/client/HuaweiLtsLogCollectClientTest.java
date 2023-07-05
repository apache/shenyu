package org.apache.shenyu.plugin.huawei.lts.client;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.huawei.lts.config.HuaweiLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public class HuaweiLtsLogCollectClientTest {
    private HuaweiLtsLogCollectClient huaweiLtsLogCollectClient;

    private final PluginData pluginData = new PluginData();

    private HuaweiLogCollectConfig.HuaweiLtsLogConfig huaweiLtsLogConfig;

    private final List<ShenyuRequestLog> logs = new ArrayList<>();

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setup() {
        this.huaweiLtsLogCollectClient = new HuaweiLtsLogCollectClient();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\n" +
                "    \"projectId\": \"3d696d796b9647f982b5ec787b2ca249\",\n" +
                "    \"logGroupId\": \"8285fbe3-9c73-4deb-a8d0-96ec06c4af3a\",\n" +
                "    \"logStreamId\": \"8b4fe176-4a7e-4bd5-8bb7-17d0ec6730b2\",\n" +
                "    \"accessKeyId\": \"FNUGUUNZL5CG6ZJDAFXX\",\n" +
                "    \"accessKeySecret\": \"g7WwPGAmCoa5Uz62lsxzHDZeSyYVNNePSjkd2jI8\",\n" +
                "    \"regionName\": \"cn-north-4\"\n" +
                "}");
        huaweiLtsLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                HuaweiLogCollectConfig.HuaweiLtsLogConfig.class);
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
        shenyuRequestLog.setRequestUri("org/apache/shenyu/plugin/logging");
        logs.add(shenyuRequestLog);
    }

    @Test
    public void testInitClient() throws NoSuchFieldException, IllegalAccessException {
        huaweiLtsLogCollectClient.initClient(huaweiLtsLogConfig);
        Field field = huaweiLtsLogCollectClient.getClass().getDeclaredField("logGroupId");
        field.setAccessible(true);
        Assertions.assertEquals(field.get(huaweiLtsLogCollectClient), "8285fbe3-9c73-4deb-a8d0-96ec06c4af3a");
        huaweiLtsLogCollectClient.close();
    }

    @Test
    public void testConsume() {
        String msg = "";
        HuaweiLogCollectConfig.INSTANCE.setHuaweiLtsLogConfig(huaweiLtsLogConfig);
        huaweiLtsLogCollectClient.initClient(huaweiLtsLogConfig);
        try {
            huaweiLtsLogCollectClient.consume(logs);
        } catch (Exception e) {
            msg = "false";
        }
        Assertions.assertEquals(msg, "");
        Assertions.assertEquals(huaweiLtsLogConfig,
                HuaweiLogCollectConfig.INSTANCE.getHuaweiLogCollectConfig());
        huaweiLtsLogCollectClient.close();
    }
}
