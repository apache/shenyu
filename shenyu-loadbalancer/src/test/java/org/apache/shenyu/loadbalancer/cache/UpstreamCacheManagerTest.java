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

package org.apache.shenyu.loadbalancer.cache;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Assertions;

import java.util.ArrayList;
import java.util.List;


/**
 * The type UpstreamCacheManager check task test.
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class UpstreamCacheManagerTest {

    private static final String SELECTOR_ID = "SELECTOR_ID";

    @Test
    @Order(1)
    public void initUpstreamCacheManagerTest() throws InterruptedException {
        final ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.getUpstreamCheck().setEnabled(true);
        shenyuConfig.getUpstreamCheck().setPrintEnabled(true);
        shenyuConfig.getUpstreamCheck().setPrintInterval(1);
        Singleton.INST.single(ShenyuConfig.class, shenyuConfig);
        Assertions.assertNotNull(UpstreamCacheManager.getInstance());
        Thread.sleep(3);
    }

    @Test
    @Order(2)
    public void submitTest() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        List<Upstream> upstreamList = new ArrayList<>(2);
        upstreamCacheManager.submit(SELECTOR_ID, upstreamList);
        upstreamList.add(Upstream.builder().url("url").status(true).build());
        upstreamList.add(Upstream.builder().status(true).build());
        upstreamCacheManager.submit(SELECTOR_ID, upstreamList);
        // hit `existUpstream.stream().filter`
        upstreamList.clear();
        upstreamList.add(Upstream.builder().url("url2").status(true).build());
        upstreamList.add(Upstream.builder().url("url").status(true).build());
        upstreamCacheManager.submit(SELECTOR_ID, upstreamList);
    }

    @Test
    @Order(3)
    public void removeByKeyTest() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        upstreamCacheManager.removeByKey(SELECTOR_ID);
    }

    @Test
    @Order(4)
    public void findUpstreamListBySelectorIdTest() {
        final UpstreamCacheManager upstreamCacheManager = UpstreamCacheManager.getInstance();
        Assertions.assertNull(upstreamCacheManager.findUpstreamListBySelectorId(SELECTOR_ID));
    }
}
