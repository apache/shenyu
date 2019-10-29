/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.config.apollo;

import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.config.api.ConfigLoader;
import org.dromara.soul.config.api.original.ServerConfigLoader;
import org.dromara.soul.config.api.original.SoulDataBase;
import org.dromara.soul.config.api.original.SoulSPI;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.function.Supplier;

import static org.mockito.Matchers.any;


/**
 * ApolloConfigLoaderTest .
 *
 * @author xiaoyu
 */
@RunWith(PowerMockRunner.class)
public class ApolloConfigLoaderTest {

    @Test
    public void testApolloLoad() throws IOException {
        ServerConfigLoader loader = new ServerConfigLoader();
        ApolloConfigLoader apolloConfigLoader = new ApolloConfigLoader();
        loader.load(ConfigLoader.Context::new, (context, config) -> {
            if (config != null) {
                if (StringUtils.isNotBlank(config.getConfigMode())) {
                    String configMode = config.getConfigMode();
                    if (configMode.equals("apollo")) {
                        apolloConfigLoader.load(context, this::test);
                    }
                }
            }
        });
        System.in.read();
    }

    private void test(Supplier supplier, ApolloConfig parent) {
        Assert.assertNotNull(parent);
        Assert.assertEquals(parent.prefix(), "soul.apollo");
        SoulDataBase config = ConfigEnv.getInstance().getConfig(SoulDataBase.class);
        Assert.assertNotNull(config);
        SoulSPI soulSPI = ConfigEnv.getInstance().getConfig(SoulSPI.class);
        Assert.assertNotNull(soulSPI);
    }

    @Test
    public void testExtension() {
        ExtensionLoader<ConfigLoader> extensionLoader = ExtensionLoader.getExtensionLoader(ConfigLoader.class);
        ConfigLoader join = extensionLoader.getJoin("apollo");
        Assert.assertEquals(join.getClass().getName(), ApolloConfigLoader.class.getName());
    }
}
