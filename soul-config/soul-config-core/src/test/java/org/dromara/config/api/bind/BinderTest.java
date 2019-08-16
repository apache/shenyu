/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.api.bind;

import org.dromara.config.core.bind.BindData;
import org.dromara.config.core.bind.Binder;
import org.dromara.config.core.bind.DataType;
import org.dromara.config.core.property.*;
import org.junit.Test;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Map;

public class BinderTest {

    @Test
    public void testBind() throws InterruptedException {
        String name = "soul.yml";
        Map<String, Object> map = new HashMap<>();
        map.put("soul.http", "123");
        map.put("soul.host", 456);
        map.put("soul.list[0]", "1234");
        map.put("soul.list[1]", "123");
        map.put("soul.array[1]", 123);
        map.put("soul.array[0]", 456);
        map.put("soul.map.ast", 456);
        map.put("soul.map.abc", 456);
        PropertyKeySource<?> propertySource = new MapPropertyKeySource(name, map);
        ConfigPropertySource configPropertySource = new DefaultConfigPropertySource(propertySource, PropertyKeyParse.INSTANCE);
        Binder binder = Binder.of(configPropertySource);
        BindData<BaseConfig> data = BindData.of(DataType.of(BaseConfig.class), BaseConfig::new);
        Object bind = binder.bind("soul", data);
//        Thread.sleep(10000);
        System.out.println(bind + "--------------->" + "1111111111111");
    }

    @Test
    public void test002() {
        Class<BaseConfig> baseConfigClass = BaseConfig.class;
        Field[] declaredFields = baseConfigClass.getDeclaredFields();
        for (Field declaredField : declaredFields) {
            declaredField.setAccessible(true);
            Type genericType = declaredField.getGenericType();
            if (genericType instanceof ParameterizedType) {
                ParameterizedType pt = (ParameterizedType) genericType;
                //得到泛型里的class类型对象
                Class<?> accountPrincipalApproveClazz = (Class<?>) pt.getActualTypeArguments()[0];
                System.out.println(accountPrincipalApproveClazz);
            }
        }
    }
}