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

package org.apache.shenyu.agent.plugin.logging.rocketmq.handler;

import org.apache.shenyu.agent.api.entity.MethodResult;
import org.apache.shenyu.agent.api.entity.TargetObject;
import org.apache.shenyu.agent.api.handler.InstanceMethodHandler;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Method interceptor is used to  collect log.
 */
public class RocketMQGlobalPluginHandler implements InstanceMethodHandler {
    
    private Field pluginsField;
    
    @SuppressWarnings("unchecked")
    @Override
    public void before(final TargetObject target, final Method method, final Object[] args, final MethodResult result) {
        final ShenyuPluginChain chain = (ShenyuPluginChain) args[1];
        if (pluginsField == null) {
            pluginsField = ReflectionUtils.findField(chain.getClass(), "plugins");
            if (pluginsField == null) {
                return;
            }
            pluginsField.setAccessible(true);
        }
        List<ShenyuPlugin> plugins = (List<ShenyuPlugin>) ReflectionUtils.getField(pluginsField, chain);
        if (plugins == null) {
            return;
        }
        Optional<ShenyuPlugin> optional = plugins.stream()
                .filter(plugin -> Objects.equals(plugin.getClass(), AgentLoggingPlugin.class)).findFirst();
        if (!optional.isPresent()) {
            plugins.add(1, new AgentLoggingPlugin());
        }
    }

}
