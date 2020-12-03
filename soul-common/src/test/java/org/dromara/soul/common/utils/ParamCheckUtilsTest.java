package org.dromara.soul.common.utils;

import org.junit.Test;
import static org.junit.Assert.assertTrue;

public class ParamCheckUtilsTest {
    @Test
    public void testDubboBodyIsEmpty() {

        assertTrue(ParamCheckUtils.dubboBodyIsEmpty(null));
        assertTrue(ParamCheckUtils.dubboBodyIsEmpty(""));
        assertTrue(ParamCheckUtils.dubboBodyIsEmpty("{}"));
        assertTrue(ParamCheckUtils.dubboBodyIsEmpty("null"));
    }
}
