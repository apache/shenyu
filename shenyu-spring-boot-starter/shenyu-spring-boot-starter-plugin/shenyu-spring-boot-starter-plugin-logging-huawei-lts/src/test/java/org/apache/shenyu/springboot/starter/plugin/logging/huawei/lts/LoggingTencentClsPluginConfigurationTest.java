package org.apache.shenyu.springboot.starter.plugin.logging.huawei.lts;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@Configuration
@EnableConfigurationProperties
public class LoggingTencentClsPluginConfigurationTest {
    @Test
    public void testLoggingPlugin() {
        new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(LoggingHuaweiLtsPluginConfiguration.class))
                .withBean(LoggingTencentClsPluginConfigurationTest.class)
                .withPropertyValues("debug=true")
                .run(context -> {
                    ShenyuPlugin plugin = context.getBean("loggingHuaweiLtsPlugin", ShenyuPlugin.class);
                    assertNotNull(plugin);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.LOGGING_HUAWEI_LTS.getName());
                });
    }
}
