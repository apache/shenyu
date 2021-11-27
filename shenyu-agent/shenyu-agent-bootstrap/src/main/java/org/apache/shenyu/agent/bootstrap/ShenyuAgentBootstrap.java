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

import org.apache.shenyu.agent.bootstrap.classloader.AgentClassLoader;

import java.lang.instrument.Instrumentation;
import java.lang.reflect.Method;

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
        AgentClassLoader agentClassLoader = AgentClassLoader.createAgentClassloader();
        Class<?> agentInstallerClass = agentClassLoader.loadClass("org.apache.shenyu.agent.AgentInstaller");
        Method agentInstallerMethod = agentInstallerClass.getMethod("installBytebuddyAgent", Instrumentation.class);
        ClassLoader originClassLoader = Thread.currentThread().getContextClassLoader();
        Thread.currentThread().setContextClassLoader(agentClassLoader);
        try {
            agentInstallerMethod.invoke(null, instrumentation);
        } finally {
            Thread.currentThread().setContextClassLoader(originClassLoader);
        }
    }
}
