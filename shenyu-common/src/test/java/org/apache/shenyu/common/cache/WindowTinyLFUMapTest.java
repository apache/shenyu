package org.apache.shenyu.common.cache;


import org.junit.Assert;
import org.junit.Test;

import java.util.Map;

/**
 * WindowTinyLFUMapTest.
 */
public class WindowTinyLFUMapTest {
    
    @Test
    public void weakKeyCache() {
        Map<String, String> map = new WindowTinyLFUMap<>(100, 100, Boolean.TRUE);
        String key1 = new String("abc");
        String key2 = new String("abc");
        map.put(key1, "1");
        map.put(key2, "1");
        Assert.assertEquals(2, map.size());
        Assert.assertEquals(key1, key2);
        Assert.assertNull(map.get("abc"));
    }
    
    @Test
    public void strongKeyCache() {
        Map<String, String> map = new WindowTinyLFUMap<>(100, 100, Boolean.FALSE);
        String key1 = new String("abc");
        String key2 = new String("abc");
        map.put(key1, "1");
        map.put(key2, "1");
        Assert.assertEquals(map.get(key1), map.get(key2));
        Assert.assertEquals(1, map.size());
    }
    
}