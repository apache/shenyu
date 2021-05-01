package org.dromara.soul.sync.data.http.refresh;

import com.google.gson.JsonObject;
import junit.framework.TestCase;
import org.dromara.soul.common.dto.ConfigData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.EnumMap;

public class DataRefreshFactoryTest extends TestCase {

    @Test
    public void testExecutor() {
        DataRefreshFactory factory = new DataRefreshFactory(null, null, null);
        EnumMap<ConfigGroupEnum, DataRefresh> map = (EnumMap<ConfigGroupEnum, DataRefresh>) ReflectionTestUtils.getField(factory, "ENUM_MAP");
        map.clear();

        Assert.assertFalse(factory.executor(new JsonObject()));

        map.put(ConfigGroupEnum.RULE, new MockDataRefresh(true));
        map.put(ConfigGroupEnum.META_DATA, new MockDataRefresh(false));
        Assert.assertTrue(factory.executor(new JsonObject()));

        map.clear();
        map.put(ConfigGroupEnum.RULE, new MockDataRefresh(false));
        map.put(ConfigGroupEnum.META_DATA, new MockDataRefresh(false));
        Assert.assertFalse(factory.executor(new JsonObject()));
    }

    private class MockDataRefresh implements DataRefresh {

        private boolean flag;

        MockDataRefresh(final boolean flag) {
            this.flag = flag;
        }

        @Override
        public Boolean refresh(final JsonObject data) {
            return flag;
        }

        @Override
        public ConfigData<?> cacheConfigData() {
            return null;
        }
    }
}
