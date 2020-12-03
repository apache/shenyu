package org.dromara.soul.plugin.sofa.handler;

import org.dromara.soul.common.config.SofaRegisterConfig;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.plugin.base.utils.Singleton;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * SofaPluginDataHandlerTest.
 *
 * @author tydhot
 */
@RunWith(MockitoJUnitRunner.class)
public class SofaPluginDataHandlerTest {

    private SofaPluginDataHandler sofaPluginDataHandler;

    private MetaData metaData;

    private String registryConfig = "{\"protocol\":\"zookeeper\",\"register\":\"127.0.0.1:2181\"}";

    @Before
    public void setUp() {
        sofaPluginDataHandler = new SofaPluginDataHandler();
        metaData = new MetaData();
        metaData.setId("1332017966661636096");
        metaData.setAppName("sofa");
        metaData.setPath("/sofa/findAll");
        metaData.setServiceName("org.dromara.soul.test.dubbo.api.service.DubboTestService");
        metaData.setMethodName("findAll");
    }

    @Test
    public void test02PluginEnable() {
        PluginData pluginData = new PluginData("", "", registryConfig, 1, true);
        sofaPluginDataHandler.handlerPlugin(pluginData);
        Assert.assertEquals(Singleton.INST.get(SofaRegisterConfig.class).getRegister(), "127.0.0.1:2181");
    }

    @Test
    public void test01PluginDisable() {
        PluginData pluginData = new PluginData("", "", registryConfig, 1, false);
        sofaPluginDataHandler.handlerPlugin(pluginData);
        Assert.assertNull(Singleton.INST.get(SofaRegisterConfig.class));
    }

}
