/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
