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

package org.apache.shenyu.plugin.motan.cache;

import com.weibo.api.motan.config.ProtocolConfig;
import com.weibo.api.motan.config.RegistryConfig;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.convert.plugin.MotanRegisterConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

/**
 * The Test Case For ApplicationConfigCache.
 */
public final class ApplicationConfigCacheTest {

    @Test
    public void testMotanParamInfo() {
        ApplicationConfigCache.MotanParamInfo motanParamInfo = new ApplicationConfigCache.MotanParamInfo(null, null);
        motanParamInfo.setParamNames(new String[]{"test"});
        motanParamInfo.setParamTypes(new Class<?>[]{ApplicationConfigCache.class});
        Assertions.assertEquals(motanParamInfo.getParamNames()[0], "test");
        Assertions.assertEquals(motanParamInfo.getParamTypes()[0], ApplicationConfigCache.class);
    }

    @Test
    public void testMotanParamExtInfo() {
        ApplicationConfigCache.MotanParamExtInfo motanParamExtInfo = new ApplicationConfigCache.MotanParamExtInfo();
        ApplicationConfigCache.MethodInfo methodInfo = new ApplicationConfigCache.MethodInfo();
        methodInfo.setMethodName("test");
        List<ApplicationConfigCache.MethodInfo> list = new ArrayList<>();
        list.add(methodInfo);
        motanParamExtInfo.setMethodInfo(list);
        motanParamExtInfo.setGroup("test");
        motanParamExtInfo.setTimeout(1000);
        Assertions.assertEquals(motanParamExtInfo.getGroup(), "test");
        Assertions.assertEquals(motanParamExtInfo.getMethodInfo().get(0).getMethodName(), "test");
        Assertions.assertEquals(motanParamExtInfo.getTimeout(), 1000);
    }

    @Test
    public void testMethodInfo() {
        List<Pair<String, String>> params = new ArrayList<>();
        Pair<String, String> pair = Pair.of("left", "right");
        params.add(pair);
        ApplicationConfigCache.MethodInfo methodInfo = new ApplicationConfigCache.MethodInfo();
        methodInfo.setParams(params);
        Assertions.assertEquals(methodInfo.getParams().get(0).getLeft(), "left");
    }

    @Test
    public void testApplicationConfigCacheInstance() {
        Assertions.assertEquals(ApplicationConfigCache.ApplicationConfigCacheInstance.INSTANCE.getClass(), ApplicationConfigCache.class);
    }

    @Test
    public void testApplicationConfigCache() throws NoSuchFieldException, IllegalAccessException {
        ApplicationConfigCache applicationConfigCache = ApplicationConfigCache.getInstance();
        Assertions.assertEquals(applicationConfigCache.getInstance().getClass(), ApplicationConfigCache.class);
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setConfig("{\"register\" : \"localhost:2181\"}");
        MotanRegisterConfig motanRegisterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), MotanRegisterConfig.class);
        applicationConfigCache.init(motanRegisterConfig);
        Field field1 = applicationConfigCache.getClass().getDeclaredField("registryConfig");
        field1.setAccessible(true);
        RegistryConfig registryConfig = (RegistryConfig) field1.get(applicationConfigCache);
        Assertions.assertEquals(registryConfig.getId(), "shenyu_motan_proxy");
        Field field2 = applicationConfigCache.getClass().getDeclaredField("protocolConfig");
        field2.setAccessible(true);
        ProtocolConfig protocolConfig = (ProtocolConfig) field2.get(applicationConfigCache);
        Assertions.assertEquals(protocolConfig.getId(), "motan2-breeze");
    }

    @Test
    public void testGet() {
        ApplicationConfigCache applicationConfigCache = ApplicationConfigCache.getInstance();
        Assertions.assertEquals(applicationConfigCache.get("/motan").toString(), "<motan:referer />");
    }
}
