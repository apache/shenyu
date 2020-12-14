package org.dromara.soul.common.constant;

import org.junit.Assert;
import org.junit.Test;

/**
 * Test cases for RedisKeyConstants
 *
 * @author DaveModl(davemo - coderpersonal @ hotmail.com)
 */
public final class RedisKeyConstantsTest {

    public static final String PLUGIN = "plugin";

    public static final String SELECTOR = "selector";

    public static final String RULE = "rule";

    public static final String PLUGIN_INFO = ":info";

    public static final String PLUGIN_SELECTOR = ":selector";


    @Test
    public void testPlugInfoKey() {
        String mockPlugin = "MockPlugin";
        String mokPluginInfoKey = RedisKeyConstants.pluginInfoKey(mockPlugin);
        Assert.assertNotNull(mockPlugin);
        Assert.assertEquals(String.join("", mockPlugin, PLUGIN_INFO), mokPluginInfoKey);
    }

    @Test
    public void testPluginSelectorKey() {
        String mockPlugin = "MockPlugin";
        String mockPluginSelectorKey = RedisKeyConstants.pluginSelectorKey(mockPlugin);
        Assert.assertNotNull(mockPlugin);
        Assert.assertEquals(String.join("", mockPlugin, PLUGIN_SELECTOR), mockPluginSelectorKey);
    }

}