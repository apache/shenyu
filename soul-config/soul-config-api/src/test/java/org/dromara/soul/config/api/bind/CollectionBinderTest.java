/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.config.api.bind;

import lombok.Data;
import org.dromara.soul.config.api.property.*;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * CollectionTest .
 * 关于集合内的测试.
 * 2019-08-16
 *
 * @author sixh
 */
public class CollectionBinderTest {


    @Test
    public void testArrayList() {
        String name = "soul.yml";
        Map<String, Object> map = new HashMap<>();
        map.put("soul.list[0]", 123);
        map.put("soul.list[1]", 234);
        PropertyKeySource<?> propertySource = new MapPropertyKeySource(name, map);
        ConfigPropertySource configPropertySource = new DefaultConfigPropertySource(propertySource, PropertyKeyParse.getInstance());
        Binder binder = Binder.of(configPropertySource);
        BindData<CollectionPojo> data = BindData.of(DataType.of(CollectionPojo.class), CollectionPojo::new);
        CollectionPojo bind = binder.bind("soul", data);
        Assert.assertEquals(bind.getList().get(0), 123);
        Assert.assertEquals(bind.getList().get(1), 234);
        System.out.println(bind);
    }

    @Test
    public void testNotSetArrayList() {
        String name = "soul.yml";
        Map<String, Object> map = new HashMap<>();
        map.put("soul.list[0]", 123);
        map.put("soul.list[1]", 234);
        PropertyKeySource<?> propertySource = new MapPropertyKeySource(name, map);
        ConfigPropertySource configPropertySource = new DefaultConfigPropertySource(propertySource, PropertyKeyParse.INSTANCE);
        Binder binder = Binder.of(configPropertySource);
        BindData<CollectionPojo2> data = BindData.of(DataType.of(CollectionPojo2.class), CollectionPojo2::new);
        CollectionPojo2 bind = binder.bind("soul", data);
        Assert.assertNull(bind);
        System.out.println(bind);
    }

    @Test
    public void testListGeneric() {
        String name = "soul.yml";
        Map<String, Object> map = new HashMap<>();
        map.put("soul.list2[0]", "123");
        map.put("soul.list2[1]", 234);
        PropertyKeySource<?> propertySource = new MapPropertyKeySource(name, map);
        ConfigPropertySource configPropertySource = new DefaultConfigPropertySource(propertySource, PropertyKeyParse.INSTANCE);
        Binder binder = Binder.of(configPropertySource);
        BindData<CollectionPojo> data = BindData.of(DataType.of(CollectionPojo.class), CollectionPojo::new);
        CollectionPojo bind = binder.bind("soul", data);
        Assert.assertEquals(bind.getList2().get(0), Integer.valueOf(123));
        Assert.assertEquals(bind.getList2().get(1), Integer.valueOf(234));
        System.out.println(bind);
    }

    @Test
    public void testArray() {
        String name = "soul.yml";
        Map<String, Object> map = new HashMap<>();
        map.put("soul.intArray[0]", 123);
        map.put("soul.intArray[1]", 234);
        PropertyKeySource<?> propertySource = new MapPropertyKeySource(name, map);
        ConfigPropertySource configPropertySource = new DefaultConfigPropertySource(propertySource, PropertyKeyParse.INSTANCE);
        Binder binder = Binder.of(configPropertySource);
        BindData<CollectionPojo> data = BindData.of(DataType.of(CollectionPojo.class), CollectionPojo::new);
        CollectionPojo bind = binder.bind("soul", data);
        Assert.assertEquals(bind.getIntArray()[0], Integer.valueOf(123));
        Assert.assertEquals(bind.getIntArray()[1], Integer.valueOf(234));
        System.out.println(bind);
    }


    @Data
    public static class CollectionPojo {

        private List list;

        private List<Integer> list2;

        private Integer[] intArray;
    }

    public static class CollectionPojo2 {
        private List list;
    }
}
