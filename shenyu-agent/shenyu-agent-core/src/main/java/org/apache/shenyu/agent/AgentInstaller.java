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

package org.apache.shenyu.agent;

import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.matcher.ElementMatcher;
import net.bytebuddy.matcher.ElementMatchers;
import org.apache.shenyu.agent.matcher.SafeErasureMatcher;
import org.apache.shenyu.agent.matcher.HasParentTypeMatcher;
import org.apache.shenyu.agent.spi.PluginAdviceDef;
import org.apache.shenyu.agent.tracing.jaeger.JaegerPluginAdviceDef;

import java.lang.instrument.Instrumentation;

import static net.bytebuddy.matcher.ElementMatchers.isMethod;
import static net.bytebuddy.matcher.ElementMatchers.named;

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
     */
    public static void installBytebuddyAgent(final Instrumentation inst) {
        // todo load config from file or cmd args
        // todo choose trace type according to config
        // todo start trace exporter according to traceType
        // todo start instrument(use spi to get advice class name)
        PluginAdviceDef pluginAdviceDef = new JaegerPluginAdviceDef();
        AgentBuilder agent = new AgentBuilder.Default()
            .disableClassFormatChanges()
            .with(AgentBuilder.RedefinitionStrategy.RETRANSFORMATION)
            .with(AgentBuilder.DescriptionStrategy.Default.POOL_ONLY);
        agent = agent.type(hasParentType(ElementMatchers.named(ABSTRACT_PLUGIN)))
                .transform(new AgentBuilder.Transformer.ForAdvice()
                        .advice(isMethod().and(named("doExecute")), pluginAdviceDef.getDoExecuteAdvice()));
        agent = agent.type(hasParentType(ElementMatchers.named(ABSTRACT_PLUGIN)))
                .transform(new AgentBuilder.Transformer.ForAdvice()
                        .advice(isMethod().and(named("execute")), pluginAdviceDef.getExecuteAdvice()));
        agent = agent.type(ElementMatchers.named(WEB_HANDLER))
                .transform(new AgentBuilder.Transformer.ForAdvice()
                        .advice(isMethod().and(named("handle")), pluginAdviceDef.getHandlerAdvice()));
        agent.installOn(inst);
    }

    /**
     * has parent class matcher.
     *
     * @param matcher the Matcher.
     * @return ElementMatcher.Junction.
     */
    public static ElementMatcher.Junction<TypeDescription> hasParentType(
            final ElementMatcher<TypeDescription> matcher) {
        return new HasParentTypeMatcher(new SafeErasureMatcher<>(matcher), false);
    }

}
