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

package org.apache.shenyu.plugin.alibaba.dubbo.handler;

import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.plugin.alibaba.dubbo.cache.AlibabaDubboConfigCache;
import org.apache.shenyu.plugin.dubbo.common.handler.AbstractDubboPluginDataHandler;

/**
 * The type Alibaba dubbo plugin data subscriber.
 */
public class AlibabaDubboPluginDataHandler extends AbstractDubboPluginDataHandler {

    @Override
    protected void initConfigCache(final DubboRegisterConfig dubboRegisterConfig) {
        AlibabaDubboConfigCache.getInstance().init(dubboRegisterConfig);
        AlibabaDubboConfigCache.getInstance().invalidateAll();
    }
}
