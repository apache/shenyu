package org.apache.shenyu.plugin.huawei.lts.collector;

import org.apache.shenyu.plugin.huawei.lts.client.HuaweiLtsLogCollectClient;
import org.apache.shenyu.plugin.huawei.lts.client.HuaweiLtsLogCollectClientTest;
import org.apache.shenyu.plugin.logging.common.client.LogConsumeClient;
import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

public class HuaweiLtsLogCollectorTest {

    private final ShenyuRequestLog shenyuRequestLog = new ShenyuRequestLog();

    @BeforeEach
    public void setUp() {
        shenyuRequestLog.setClientIp("0.0.0.0");
        shenyuRequestLog.setPath("org/apache/shenyu/plugin/logging");
    }

    @Test
    public void testAbstractLogCollector() throws Exception {
        HuaweiLtsLogCollector.getInstance().start();
        Field field1 = AbstractLogCollector.class.getDeclaredField("started");
        field1.setAccessible(true);
        Assertions.assertEquals(field1.get(HuaweiLtsLogCollector.getInstance()).toString(), "true");
        HuaweiLtsLogCollector.getInstance().collect(shenyuRequestLog);
        HuaweiLtsLogCollector.getInstance().close();
        Field field2 = AbstractLogCollector.class.getDeclaredField("started");
        field2.setAccessible(true);
        Assertions.assertEquals(field2.get(HuaweiLtsLogCollector.getInstance()).toString(), "false");
    }

    @Test
    public void testGetLogConsumeClient() {
        LogConsumeClient logConsumeClient = new HuaweiLtsLogCollector().getLogConsumeClient();
        Assertions.assertEquals(HuaweiLtsLogCollectClient.class, logConsumeClient.getClass());
    }
}
