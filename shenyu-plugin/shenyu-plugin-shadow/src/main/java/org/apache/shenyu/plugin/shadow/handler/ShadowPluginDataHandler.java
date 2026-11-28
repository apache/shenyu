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

package org.apache.shenyu.plugin.shadow.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.shadow.config.ShadowConfigValidator;
import org.apache.shenyu.plugin.shadow.dispatch.ShadowRuntimeRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Optional;

/**
 * Receives shadow rule updates from the data-sync channel.
 *
 * <p>A rule is cached only after its bounds are valid. An invalid update removes
 * the previous runtime instead of leaving a partially updated rule active.</p>
 */
public class ShadowPluginDataHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(ShadowPluginDataHandler.class);

    private final ShadowRuntimeRegistry runtimeRegistry;

    public ShadowPluginDataHandler(final ShadowRuntimeRegistry runtimeRegistry) {
        this.runtimeRegistry = runtimeRegistry;
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        String key = CacheKeyUtils.INST.getKey(ruleData);
        if (Objects.isNull(ruleData.getHandle())) {
            runtimeRegistry.remove(key);
            return;
        }
        ShadowHandle handle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), ShadowHandle.class);
        Optional<String> problem = ShadowConfigValidator.validate(handle);
        if (problem.isPresent()) {
            LOG.warn("ignore invalid shadow rule {}: {}", ruleData.getId(), problem.get());
            runtimeRegistry.remove(key);
            return;
        }
        runtimeRegistry.register(key, handle);
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(data -> runtimeRegistry.remove(CacheKeyUtils.INST.getKey(data)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SHADOW.getName();
    }
}
