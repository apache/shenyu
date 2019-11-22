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

package org.dromara.soul.register.dubbo64;

import com.alibaba.dubbo.common.URL;
import java.util.ArrayList;
import java.util.List;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.register.DubboConfig;
import org.dromara.soul.register.api.RegisterDirectory;
import org.junit.Test;

/**
 * Created by apa7 on 2019/11/20.
 */
public class DubboRegisterDirectoryTest {

    @Test
    public void test001() throws InterruptedException {
        DubboConfig config = new DubboConfig();
        List<String> urls = new ArrayList<>();
        urls.add("zookeeper://192.168.1.84:2181?client=curator");
        config.setRegistry(urls);
        ConfigEnv.getInstance().putBean(config);
        RegisterDirectory directory = new DubboRegisterDirectory(URL.valueOf("zookeeper://192.168.1.84:2181?client=curator"), System.out::println);
        Thread.sleep(Integer.MAX_VALUE);
    }
}