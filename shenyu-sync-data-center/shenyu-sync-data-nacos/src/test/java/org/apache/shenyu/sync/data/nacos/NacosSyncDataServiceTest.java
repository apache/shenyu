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

package org.apache.shenyu.sync.data.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import org.apache.shenyu.sync.data.nacos.handler.NacosMockConfigService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;

/**
 * add test case for {@link NacosSyncDataService}.
 */
public final class NacosSyncDataServiceTest {

    private NacosSyncDataService nacosSyncDataService;

    @BeforeEach
    public void setup() {
        ConfigService configService = new NacosMockConfigService();
        nacosSyncDataService = new NacosSyncDataService(configService, null,
                Collections.emptyList(), Collections.emptyList());
    }

    @Test
    public void testStart() {
        nacosSyncDataService.start();
    }

    @Test
    public void testClose() {
        nacosSyncDataService.close();
    }
}
