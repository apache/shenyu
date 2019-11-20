/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

import org.dromara.soul.common.http.URL;
import org.dromara.soul.register.api.RegisterConst;
import org.dromara.soul.register.api.Registry;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by apa7 on 2019/11/20.
 */
public class ZookeeperRegistryTest {

    private String address = "zookeeper://192.168.1.84:2181?cluster=192.168.1.85:2181,192.168.1.86:2181";

    Registry registry;

    @Before
    public void setUp() {
        registry = new ZookeeperRegistry(URL.parse(address));
    }

    @Test
    public void register() {
        String url = "soul://192.168.1.100:8888/soul/server?" + RegisterConst.EPHEMERAL_KEY + "=false";
        registry.register(URL.parse(url));
    }

    @Test
    public void unregister() {
        String url = "soul://192.168.1.100:8888/soul/server?" + RegisterConst.EPHEMERAL_KEY + "=false";
        registry.unregister(URL.parse(url));
    }
}