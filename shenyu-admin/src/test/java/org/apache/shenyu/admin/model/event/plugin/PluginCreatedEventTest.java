package org.apache.shenyu.admin.model.event.plugin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * test cast for {@link PluginCreatedEvent}.
 */
public class PluginCreatedEventTest {

    private PluginDO pluginDO;

    private PluginCreatedEvent pluginCreateEvent;

    @BeforeEach
    public void setUp() {
        pluginDO = PluginDO.builder()
                .id("1")
                .name("test-plugin")
                .config("{\"config\":\"test\"}")
                .enabled(false)
                .role("Test")
                .sort(1)
                .build();
        pluginCreateEvent = new PluginCreatedEvent(pluginDO, "test-operator");
    }

    @Test
    public void createPluginBuildContextTest() {
        String context =
                String.format("the plugin [%s] is %s", pluginDO.getName(), StringUtils.lowerCase(EventTypeEnum.PLUGIN_CREATE.getType().toString()));
        assertEquals(context, pluginCreateEvent.buildContext());
    }

    @Test
    public void getSourceTest() {
        assertEquals(pluginDO, pluginCreateEvent.getSource());
    }
}
