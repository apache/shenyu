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

package org.apache.shenyu.agent.core;

import net.bytebuddy.agent.builder.AgentBuilder;
import org.apache.shenyu.agent.core.bytebuddy.TransformListener;
import org.apache.shenyu.agent.spi.PluginAdviceDef;
import org.apache.shenyu.spi.ExtensionLoader;

import java.lang.instrument.Instrumentation;

import static net.bytebuddy.matcher.ElementMatchers.isMethod;
import static net.bytebuddy.matcher.ElementMatchers.named;
import static org.apache.shenyu.agent.core.bytebuddy.HasParentTypeMatcher.hasParentType;

/**
 * AgentInstaller.
 */
public class AgentInstaller {

    private static final String ABSTRACT_PLUGIN = "org.apache.shenyu.plugin.base.AbstractShenyuPlugin";

    private static final String WEB_HANDLER = "org.apache.shenyu.web.handler.ShenyuWebHandler";

    /**
     * start to install agent.
     *
     * @param inst the instrumentation.
     * @param classLoader custom classloader
     */
    public static void installBytebuddyAgent(final Instrumentation inst, final ClassLoader classLoader) {
        // todo start trace exporter according to traceType
        String traceType = System.getProperty("shenyu.agent.trace", "jaeger");
        PluginAdviceDef pluginAdviceDef = ExtensionLoader.getExtensionLoader(PluginAdviceDef.class).getJoin(traceType);

        AgentBuilder agent = new AgentBuilder.Default()
                .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
                .with(AgentBuilder.DescriptionStrategy.Default.POOL_ONLY)
                .with(new TransformListener());

        agent = agent.type(hasParentType(named(ABSTRACT_PLUGIN)))
                .transform(new AgentBuilder.Transformer.ForAdvice()
                        .include(classLoader)
                        .with(AgentBuilder.LocationStrategy.NoOp.INSTANCE)
                        .advice(isMethod().and(named("execute")), pluginAdviceDef.getExecuteAdvice())
                        .advice(isMethod().and(named("doExecute")), pluginAdviceDef.getDoExecuteAdvice()));
        agent = agent.type(named(WEB_HANDLER))
                .transform(new AgentBuilder.Transformer.ForAdvice()
                        .include(classLoader)
                        .advice(isMethod().and(named("handle")), pluginAdviceDef.getHandlerAdvice()));
        agent.installOn(inst);
    }
}
