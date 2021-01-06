package org.dromara.soul.spring.boot.starter.plugin.alibaba.dubbo;

import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.alibaba.dubbo.AlibabaDubboPlugin;
import org.dromara.soul.plugin.alibaba.dubbo.handler.AlibabaDubboPluginDataHandler;
import org.dromara.soul.plugin.alibaba.dubbo.param.BodyParamPlugin;
import org.dromara.soul.plugin.alibaba.dubbo.response.DubboResponsePlugin;
import org.dromara.soul.plugin.alibaba.dubbo.subscriber.AlibabaDubboMetaDataSubscriber;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Test case for {@link AlibabaDubboPluginConfiguration}.
 *
 * @author: ZhouBin
 */
@RunWith(SpringRunner.class)
@SpringBootTest(
        classes = {
                AlibabaDubboPluginConfiguration.class,
                AlibabaDubboPluginConfigurationTest.class
        },
        webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT
)
@EnableAutoConfiguration
public class AlibabaDubboPluginConfigurationTest {

    @Autowired
    private AlibabaDubboPlugin alibabaDubboPlugin;

    @Autowired
    private BodyParamPlugin bodyParamPlugin;

    @Autowired
    private DubboResponsePlugin dubboResponsePlugin;

    @Autowired
    private AlibabaDubboPluginDataHandler alibabaDubboPluginDataHandler;

    @Autowired
    private AlibabaDubboMetaDataSubscriber alibabaDubboMetaDataSubscriber;

    @Test
    public void testAlibabaDubboPlugin() {
        Assert.assertEquals(PluginEnum.DUBBO.getCode(), alibabaDubboPlugin.getOrder());
        Assert.assertEquals(PluginEnum.DUBBO.getName(), alibabaDubboPlugin.named());

        Assert.assertEquals(PluginEnum.DUBBO.getCode() - 1, bodyParamPlugin.getOrder());
        Assert.assertEquals("alibaba-dubbo-body-param", bodyParamPlugin.named());

        Assert.assertEquals(PluginEnum.RESPONSE.getCode(), dubboResponsePlugin.getOrder());
        Assert.assertEquals(PluginEnum.RESPONSE.getName(), dubboResponsePlugin.named());

        Assert.assertEquals(PluginEnum.DUBBO.getName(), alibabaDubboPluginDataHandler.pluginNamed());

        Assert.assertNotNull(alibabaDubboMetaDataSubscriber);
    }
}
