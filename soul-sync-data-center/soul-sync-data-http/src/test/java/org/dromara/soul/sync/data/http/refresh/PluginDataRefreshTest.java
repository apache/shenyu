package org.dromara.soul.sync.data.http.refresh;

import com.google.gson.JsonObject;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test cases for {@link PluginDataRefresh}
 *
 * @author davidliu
 */
public class PluginDataRefreshTest {
    
    private final PluginDataRefresh mockPluginDataRefresh = new PluginDataRefresh(new PluginDataSubscriber() {
        @Override
        public void onSubscribe(PluginData pluginData) {
        
        }
    });
    
    /**
     * test case for {@link PluginDataRefresh#convert(JsonObject)}
     */
    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.PLUGIN.name(), expectJsonObject);
        Assert.assertEquals(expectJsonObject, mockPluginDataRefresh.convert(jsonObject));
    }
    
    /**
     * test case for {@link PluginDataRefresh#fromJson(JsonObject)}
     */
    @Test
    public void testFromJson() {
        ConfigData<PluginData> pluginDataConfigData = new ConfigData<>();
        PluginData pluginData = new PluginData();
        pluginDataConfigData.setData(Collections.singletonList(pluginData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(pluginDataConfigData), JsonObject.class);
        Assert.assertEquals(pluginDataConfigData, mockPluginDataRefresh.fromJson(jsonObject));
    }
    
    /**
     * This case coverages the following method:
     * {@link PluginDataRefresh#cacheConfigData()}
     * {@link PluginDataRefresh#updateCacheIfNeed(ConfigData)}
     * <p>
     * For {@link SelectorDataRefresh} inherits from {@link AbstractDataRefresh}, the {@link AbstractDataRefresh#GROUP_CACHE} was initialized when the class of
     * {@link AbstractDataRefresh} load, in two different test methods in this class, the the {@link AbstractDataRefresh#GROUP_CACHE} class only load once, so
     * the method which manipulate the {@link AbstractDataRefresh#GROUP_CACHE} invocation has aftereffects to the other methods
     */
    @Test
    public void testUpdateCacheIfNeed() {
        final PluginDataRefresh pluginDataRefresh = mockPluginDataRefresh;
        // first, expect getting null from cache
        Assert.assertNull(pluginDataRefresh.cacheConfigData());
        // update cache, then assert equals
        ConfigData<PluginData> expect = new ConfigData<>();
        Assert.assertTrue(pluginDataRefresh.updateCacheIfNeed(expect));
        Assert.assertEquals(expect, pluginDataRefresh.cacheConfigData());
    }
    
    /**
     * This case is only for {@link PluginDataRefresh} code coverage
     */
    @Test
    public void testRefreshCoverage() {
        final PluginDataRefresh pluginDataRefresh = mockPluginDataRefresh;
        PluginData selectorData = new PluginData();
        List<PluginData> selectorDataList = new ArrayList<>();
        pluginDataRefresh.refresh(selectorDataList);
        selectorDataList.add(selectorData);
        pluginDataRefresh.refresh(selectorDataList);
        
    }
}