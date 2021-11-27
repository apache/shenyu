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

package org.apache.shenyu.agent.bootstrap.classloader;

import org.apache.shenyu.agent.bootstrap.ShenyuAgentBootstrap;
import org.apache.shenyu.agent.bootstrap.exception.AgentBootstrapFailException;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.CodeSource;
import java.util.Arrays;
import java.util.Objects;

/**
 * The agent class loader.
 */
public class AgentClassLoader extends URLClassLoader {

    public AgentClassLoader(final URL[] urls, final ClassLoader agentParent) {
        super(urls, agentParent);
    }

    /**
     * Get the agent class loader.
     *
     * @return AgentClassLoader agent class loader
     * @throws AgentBootstrapFailException the exception
     */
    public static AgentClassLoader createAgentClassloader() throws AgentBootstrapFailException {
        CodeSource codeSource = ShenyuAgentBootstrap.class.getProtectionDomain().getCodeSource();
        if (codeSource == null) {
            throw new AgentBootstrapFailException("could not get agent jar location");
        }
        try {
            File javaagentFile = new File(codeSource.getLocation().toURI());
            if (!javaagentFile.isFile()) {
                throw new AgentBootstrapFailException("agent jar location doesn't appear to be a file: " + javaagentFile.getAbsolutePath());
            }
            File pluginFileDirectory = new File(javaagentFile.getParent() + "/plugins");
            File[] jars = Arrays.stream(Objects.requireNonNull(pluginFileDirectory.listFiles()))
                    .filter(filePointer -> filePointer.getName().endsWith(".jar")).toArray(File[]::new);
            URL[] urls = new URL[jars.length + 1];
            for (int i = 0; i < urls.length - 1; i++) {
                urls[i] = jars[i].toURI().toURL();
            }
            urls[jars.length] = javaagentFile.toURI().toURL();
            return new AgentClassLoader(urls, null);
        } catch (URISyntaxException | MalformedURLException e) {
            throw new AgentBootstrapFailException("failed to construct the agent classloader dur to: " + e.getMessage());
        }
    }
}
