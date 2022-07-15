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

package org.apache.shenyu.web.loader;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

/**
 * Test for ShenyuLoaderServiceTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ShenyuLoaderServiceTest {

    @Test
    public void loaderExtPluginsTest() {
        final ShenyuConfig.ExtPlugin extPlugin = new ShenyuConfig.ExtPlugin();
        extPlugin.setEnabled(true);
        extPlugin.setScheduleDelay(0);
        extPlugin.setScheduleTime(2);
        final ShenyuConfig shenyuConfig = new ShenyuConfig();
        shenyuConfig.setExtPlugin(extPlugin);
        new ShenyuLoaderService(null, null, shenyuConfig);
    }
}
