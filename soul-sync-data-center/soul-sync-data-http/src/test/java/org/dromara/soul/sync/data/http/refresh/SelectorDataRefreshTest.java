package org.dromara.soul.sync.data.http.refresh;

import com.google.gson.JsonObject;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test for {@link SelectorDataRefresh}
 *
 * @author davidliu
 */
public class SelectorDataRefreshTest {
    
    private final SelectorDataRefresh mockSelectorDataRefresh = new SelectorDataRefresh(new PluginDataSubscriber() {
        @Override
        public void onSubscribe(PluginData pluginData) {
        
        }
    });
    
    
    /**
     * test case for {@link SelectorDataRefresh#convert(JsonObject)}
     */
    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.SELECTOR.name(), expectJsonObject);
        Assert.assertEquals(expectJsonObject, mockSelectorDataRefresh.convert(jsonObject));
    }
    
    /**
     * test case for {@link SelectorDataRefresh#fromJson(JsonObject)}
     */
    @Test
    public void testFromJson() {
        ConfigData<SelectorData> selectorDataConfigData = new ConfigData<>();
        SelectorData selectorData = new SelectorData();
        selectorDataConfigData.setData(Collections.singletonList(selectorData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(selectorDataConfigData), JsonObject.class);
        Assert.assertEquals(selectorDataConfigData, mockSelectorDataRefresh.fromJson(jsonObject));
    }
    
    /**
     * This case coverages the following method:
     * * updateCacheIfNeed
     * * cacheConfigData
     * For {@link SelectorDataRefresh} inherits from {@link AbstractDataRefresh}, the {@link AbstractDataRefresh#GROUP_CACHE} was initialized when the class of
     * {@link AbstractDataRefresh} load, in two different test methods in this class, the the {@link AbstractDataRefresh#GROUP_CACHE} class only load once, so
     * the method which manipulate the {@link AbstractDataRefresh#GROUP_CACHE} invocation has aftereffects to the other methods
     */
    @Test
    public void testUpdateCacheIfNeed() {
        final SelectorDataRefresh selectorDataRefresh = mockSelectorDataRefresh;
        // first, expect getting null from cache
        Assert.assertNull(selectorDataRefresh.cacheConfigData());
        // update cache, then assert equals
        ConfigData<SelectorData> expect = new ConfigData<>();
        Assert.assertTrue(selectorDataRefresh.updateCacheIfNeed(expect));
        Assert.assertEquals(expect, selectorDataRefresh.cacheConfigData());
    }
    
    /**
     * This case is only for {@link SelectorDataRefresh} code coverage
     */
    @Test
    public void testRefreshCoverage() {
        final SelectorDataRefresh selectorDataRefresh = mockSelectorDataRefresh;
        SelectorData selectorData = new SelectorData();
        List<SelectorData> selectorDataList = new ArrayList<>();
        selectorDataRefresh.refresh(selectorDataList);
        selectorDataList.add(selectorData);
        selectorDataRefresh.refresh(selectorDataList);
        
    }
}