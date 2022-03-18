package org.apache.shenyu.plugin.cache.base.redis.handler;


import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.RedisModeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.cache.base.ICache;
import org.apache.shenyu.plugin.cache.base.config.CacheConfig;
import org.apache.shenyu.plugin.cache.base.enums.CacheEnum;
import org.apache.shenyu.plugin.cache.base.handler.CacheHandler;
import org.apache.shenyu.plugin.cache.base.redis.ShenyuCacheReactiveRedisTemplate;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * CacheHandlerTest
 */
@ExtendWith(MockitoExtension.class)
public class CacheHandlerTest {

    private ICache iCache;

    @Before
    public void prepare() {

        final CacheConfig cacheConfig = new CacheConfig();
        cacheConfig.setCacheType(CacheEnum.REDIS.getName());
        cacheConfig.setUrl("shenyu-redis:6379");
        cacheConfig.setMode(RedisModeEnum.STANDALONE.getName());
        final CacheHandler cacheHandler = new CacheHandler();
        final PluginData pluginData = new PluginData();
        pluginData.setConfig(GsonUtils.getInstance().toJson(cacheConfig));
        cacheHandler.handlerPlugin(pluginData);
        iCache = Singleton.INST.get(ShenyuCacheReactiveRedisTemplate.class);

    }

    @Test
    public void testCacheHandler() {

        final String testKey = "testCacheHandler";
        assertEquals(Boolean.FALSE, iCache.isExist(testKey));
        boolean flag = iCache.cacheData(testKey, testKey.getBytes(StandardCharsets.UTF_8), 1000);
        assertEquals(Boolean.TRUE, flag);
        assertEquals(Boolean.TRUE, iCache.isExist(testKey));
        final byte[] value = iCache.getData(testKey);
        assert null != value;
        assertEquals(testKey, new String(value, StandardCharsets.UTF_8));
    }
}
