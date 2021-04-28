package org.dromara.soul.springboot.starter.plugin.debug;

import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.plugin.api.SoulPlugin;
import org.dromara.soul.plugin.debug.DebugPlugin;
import org.junit.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Test case for {@link DebugPluginConfiguration}..
 *
 * @author xuxd
 **/
public class DebugPluginConfigurationTest {

    @Test
    public void testDebugPlugin() {
        new ApplicationContextRunner()
            .withConfiguration(
                AutoConfigurations.of(
                    DebugPlugin.class
                ))
            .withPropertyValues("debug=true")
            .run(
                context -> {
                    SoulPlugin plugin = context.getBean("debugPlugin", SoulPlugin.class);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.DEBUG.getName());
                }
            );
    }
}
