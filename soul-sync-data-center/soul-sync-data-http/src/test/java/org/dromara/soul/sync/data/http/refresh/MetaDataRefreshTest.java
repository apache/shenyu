package org.dromara.soul.sync.data.http.refresh;

import com.google.gson.JsonObject;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test cases for {@link MetaDataRefresh}
 *
 * @author davidliu
 */
public class MetaDataRefreshTest {
    
    private final MetaDataRefresh mockMetaDataRefresh = this.buildMockMetaDataRefresh();
    
    /**
     * test case for {@link MetaDataRefresh#convert(JsonObject)}
     */
    @Test
    public void testConvert() {
        JsonObject jsonObject = new JsonObject();
        JsonObject expectJsonObject = new JsonObject();
        jsonObject.add(ConfigGroupEnum.META_DATA.name(), expectJsonObject);
        Assert.assertEquals(expectJsonObject, mockMetaDataRefresh.convert(jsonObject));
    }
    
    /**
     * test case for {@link MetaDataRefresh#fromJson(JsonObject)}
     */
    @Test
    public void testFromJson() {
        ConfigData<MetaData> metaDataConfigData = new ConfigData<>();
        MetaData metaData = new MetaData();
        metaDataConfigData.setData(Collections.singletonList(metaData));
        JsonObject jsonObject = GsonUtils.getGson().fromJson(GsonUtils.getGson().toJson(metaDataConfigData), JsonObject.class);
        Assert.assertEquals(metaDataConfigData, mockMetaDataRefresh.fromJson(jsonObject));
    }
    
    /**
     * This case coverages the following method:
     * {@link MetaDataRefresh#cacheConfigData()}
     * {@link MetaDataRefresh#updateCacheIfNeed(ConfigData)}
     * <p>
     * For {@link SelectorDataRefresh} inherits from {@link AbstractDataRefresh}, the {@link AbstractDataRefresh#GROUP_CACHE} was initialized when the class of
     * {@link AbstractDataRefresh} load, in two different test methods in this class, the the {@link AbstractDataRefresh#GROUP_CACHE} class only load once, so
     * the method which manipulate the {@link AbstractDataRefresh#GROUP_CACHE} invocation has aftereffects to the other methods
     */
    @Test
    public void testUpdateCacheIfNeed() {
        final MetaDataRefresh metaDataRefresh = mockMetaDataRefresh;
        // first, expect getting null from cache
        Assert.assertNull(metaDataRefresh.cacheConfigData());
        // update cache, then assert equals
        ConfigData<MetaData> expect = new ConfigData<>();
        Assert.assertTrue(metaDataRefresh.updateCacheIfNeed(expect));
        Assert.assertEquals(expect, metaDataRefresh.cacheConfigData());
    }
    
    /**
     * This case is only for {@link MetaDataRefresh} code coverage
     */
    @Test
    public void testRefreshCoverage() {
        final MetaDataRefresh metaDataRefresh = mockMetaDataRefresh;
        MetaData metaData = new MetaData();
        List<MetaData> metaDataList = new ArrayList<>();
        metaDataRefresh.refresh(metaDataList);
        metaDataList.add(metaData);
        metaDataRefresh.refresh(metaDataList);
        
    }
    
    private MetaDataRefresh buildMockMetaDataRefresh() {
        List<MetaDataSubscriber> metaDataSubscribers = new ArrayList<>();
        metaDataSubscribers.add(new MetaDataSubscriber() {
            @Override
            public void onSubscribe(MetaData metaData) {
            
            }
            
            @Override
            public void unSubscribe(MetaData metaData) {
            
            }
        });
        return new MetaDataRefresh(metaDataSubscribers);
    }
    
}