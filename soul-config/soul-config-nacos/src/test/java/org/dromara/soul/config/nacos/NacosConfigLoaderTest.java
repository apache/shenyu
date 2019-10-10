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

package org.dromara.soul.config.nacos;

import java.io.ByteArrayInputStream;
import java.util.function.Supplier;
import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.config.api.ConfigLoader;
import org.dromara.soul.config.api.original.ServerConfigLoader;
import org.dromara.soul.config.api.original.SoulDataBase;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import static org.mockito.Matchers.any;


/**
 * NacosConfigLoaderTest .
 * nacos config test.
 * 2019/8/17
 *
 * @author sixh
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(NacosClient.class)
public class NacosConfigLoaderTest {
    private NacosClient client = PowerMockito.mock(NacosClient.class);

    @Before
    public void setUp() {
        String str = "soul.database.url=jdbc:mysql://127.0.0.1:3306/calvin_account?useUnicode=true&characterEncoding=UTF-8&zeroDateTimeBehavior=convertToNull&useSSL=false\n" +
                     "soul.database.userName=root\n" +
                     "soul.database.password=root";
        ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(str.getBytes());
        try {
            PowerMockito.when(client.pull(any())).thenReturn(byteArrayInputStream);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * nacos load.
     */
    @Test
    public void testNacosLoad() {
        ServerConfigLoader loader = new ServerConfigLoader();
        NacosConfigLoader nacosConfigLoader = new NacosConfigLoader(client);
        loader.load(ConfigLoader.Context::new, (context, config) -> {
            if (config != null) {
                if (StringUtils.isNotBlank(config.getConfigMode())) {
                    String configMode = config.getConfigMode();
                    if (configMode.equals("nacos")) {
                        nacosConfigLoader.load(context, this::test);
                    }
                }
            }
        });
    }

    private void test(Supplier supplier, NacosConfig parent) {
        Assert.assertNotNull(parent);
        Assert.assertEquals(parent.prefix(), "soul.nacos");
        SoulDataBase config = ConfigEnv.getInstance().getConfig(SoulDataBase.class);
        Assert.assertNotNull(config);
        Assert.assertEquals(config.getUrl(), "jdbc:mysql://127.0.0.1:3306/calvin_account?useUnicode=true&characterEncoding=UTF-8&zeroDateTimeBehavior=convertToNull&useSSL=false");
        Assert.assertEquals(config.getUserName(), "root");
        Assert.assertEquals(config.getPassword(), "root");
    }

    @Test
    public void testExtension() {
        ExtensionLoader<ConfigLoader> extensionLoader = ExtensionLoader.getExtensionLoader(ConfigLoader.class);
        ConfigLoader join = extensionLoader.getJoin("nacos");
        Assert.assertEquals(join.getClass().getName(), NacosConfigLoader.class.getName());
    }
}
