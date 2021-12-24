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

package org.apache.shenyu.agent.bootstrap;

import net.bytebuddy.ByteBuddy;
import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.dynamic.scaffold.TypeValidation;
import net.bytebuddy.matcher.ElementMatchers;
import org.apache.shenyu.agent.core.bytebuddy.listener.TransformListener;
import org.apache.shenyu.agent.core.bytebuddy.matcher.ShenyuAgentTypeMatcher;
import org.apache.shenyu.agent.core.bytebuddy.transformer.ShenyuAgentTransformer;
import org.apache.shenyu.agent.core.loader.ShenyuAgentConfigLoader;
import org.apache.shenyu.agent.core.loader.ShenyuAgentPluginLoader;
import org.apache.shenyu.agent.core.plugin.PluginLifecycleManager;
import org.apache.shenyu.agent.core.utils.ShenyuAgentConfigUtils;

import java.lang.instrument.Instrumentation;

/**
 * The type Shenyu agent bootstrap.
 */
public class ShenyuAgentBootstrap {
    
    /**
     * Premain for instrumentation.
     *
     * @param arguments arguments
     * @param instrumentation instrumentation
     * @throws Exception the exception
     */
    public static void premain(final String arguments, final Instrumentation instrumentation) throws Exception {
        ShenyuAgentConfigUtils.setConfig(ShenyuAgentConfigLoader.load());
        ShenyuAgentPluginLoader.getInstance().loadAllPlugins();
        AgentBuilder agentBuilder = new AgentBuilder.Default().with(new ByteBuddy().with(TypeValidation.ENABLED))
                .ignore(ElementMatchers.isSynthetic())
                .or(ElementMatchers.nameStartsWith("org.apache.shenyu.agent."));
        agentBuilder.type(ShenyuAgentTypeMatcher.getInstance())
                .transform(new ShenyuAgentTransformer())
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(new TransformListener()).installOn(instrumentation);
        PluginLifecycleManager lifecycleManager = new PluginLifecycleManager();
        lifecycleManager.startup(ShenyuAgentConfigUtils.getPluginConfigMap());
        Runtime.getRuntime().addShutdownHook(new Thread(lifecycleManager::close));
    }
}
