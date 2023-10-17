package org.apache.shenyu.admin.model.event.plugin;


import static org.junit.jupiter.api.Assertions.assertEquals;
import java.sql.Timestamp;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * test cast for {@link PluginChangedEvent}.
 */
public class PluginChangedEventTest {

    private PluginDO pluginDO;

    private PluginDO withoutChangePluginDO;

    private PluginDO changePluginDO;

    @BeforeEach
    public void setUp() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        pluginDO = PluginDO.builder()
                .id("1")
                .name("test-plugin")
                .config("{\"config\":\"test\"}")
                .enabled(false)
                .role("Test")
                .sort(1)
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        withoutChangePluginDO = PluginDO.builder()
                .id("1")
                .name("test-plugin")
                .config("{\"config\":\"test\"}")
                .enabled(false)
                .role("Test")
                .sort(1)
                .dateUpdated(now)
                .dateCreated(now)
                .build();
        changePluginDO = PluginDO.builder()
                .id("1")
                .name("test-plugin-change")
                .config("{\"config\":\"test-change\"}")
                .enabled(true)
                .role("TestChange")
                .sort(2)
                .dateUpdated(now)
                .dateCreated(now)
                .build();
    }

    @Test
    public void deletePluginBuildContextTest() {
        PluginChangedEvent pluginDeleteEvent = new PluginChangedEvent(pluginDO, null, EventTypeEnum.PLUGIN_DELETE, "test-operator");
        String context =
                String.format("the plugin [%s] is %s", pluginDO.getName(), StringUtils.lowerCase(EventTypeEnum.PLUGIN_DELETE.getType().toString()));
        assertEquals(context, pluginDeleteEvent.buildContext());
    }

    @Test
    public void updatePluginBuildContextTest() {
        String eventTypeStr = StringUtils.lowerCase(EventTypeEnum.PLUGIN_UPDATE.getType().toString());
        PluginChangedEvent pluginUpdateEventWithoutChange = new PluginChangedEvent(pluginDO, pluginDO, EventTypeEnum.PLUGIN_UPDATE, "test-operator");
        String withoutChangeContrast = "it no change";
        String context =
                String.format("the plugin [%s] is %s : %s", pluginDO.getName(), eventTypeStr, withoutChangeContrast);
        assertEquals(context, pluginUpdateEventWithoutChange.buildContext());

        PluginChangedEvent pluginUpdateEventNotSameDO = new PluginChangedEvent(withoutChangePluginDO, pluginDO, EventTypeEnum.PLUGIN_UPDATE, "test-operator");
        assertEquals(context, pluginUpdateEventNotSameDO.buildContext());

        PluginChangedEvent pluginUpdateEventChange = new PluginChangedEvent(changePluginDO, pluginDO, EventTypeEnum.PLUGIN_UPDATE, "test-operator");
        final StringBuilder contrast = new StringBuilder();
        contrast.append(String.format("name[%s => %s] ", pluginDO.getName(), changePluginDO.getName()));
        contrast.append(String.format("config[%s => %s] ", pluginDO.getConfig(), changePluginDO.getConfig()));
        contrast.append(String.format("role[%s => %s] ", pluginDO.getRole(), changePluginDO.getRole()));
        contrast.append(String.format("enable[%s => %s] ", pluginDO.getEnabled(), changePluginDO.getEnabled()));
        contrast.append(String.format("sort[%s => %s] ", pluginDO.getSort(), changePluginDO.getSort()));
        String changeContext = String.format("the plugin [%s] is %s : %s", changePluginDO.getName(), eventTypeStr, contrast);
        assertEquals(changeContext, pluginUpdateEventChange.buildContext());
    }
}
