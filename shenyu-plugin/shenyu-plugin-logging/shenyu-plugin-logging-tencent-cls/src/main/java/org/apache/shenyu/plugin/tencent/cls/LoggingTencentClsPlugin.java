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

package org.apache.shenyu.plugin.tencent.cls;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.common.AbstractLoggingPlugin;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.tencent.cls.collector.TencentClsSlsLogCollector;

/**
 * LoggingTencentClsPlugin send log to Tencent cls service.
 */
public class LoggingTencentClsPlugin extends AbstractLoggingPlugin {

    @Override
    protected LogCollector logCollector() {
        return TencentClsSlsLogCollector.getInstance();
    }

    /**
     * pluginEnum.
     *
     * @return plugin
     */
    @Override
    public PluginEnum pluginEnum() {
        return PluginEnum.LOGGING_TENCENT_CLS;
    }
}
