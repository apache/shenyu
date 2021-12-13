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

import org.apache.shenyu.agent.core.exception.AgentBootstrapFailException;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
     * @throws org.apache.shenyu.agent.core.exception.AgentBootstrapFailException the exception
     */
    public static AgentClassLoader createAgentClassloader() throws org.apache.shenyu.agent.core.exception.AgentBootstrapFailException {
        CodeSource codeSource = AgentClassLoader.class.getProtectionDomain().getCodeSource();
        if (codeSource == null) {
            throw new org.apache.shenyu.agent.core.exception.AgentBootstrapFailException("could not get agent jar location");
        }
        try {
            File javaagentFile = new File(codeSource.getLocation().toURI());
            if (!javaagentFile.isFile()) {
                throw new org.apache.shenyu.agent.core.exception.AgentBootstrapFailException("agent jar location doesn't appear to be a file: " + javaagentFile.getAbsolutePath());
            }

            File pluginFileDirectory = new File(javaagentFile.getParent() + "/plugins");
            File libsFileDirectory = new File(javaagentFile.getParent() + "/libs");

            File[] pluginJars = Arrays.stream(Objects.requireNonNull(pluginFileDirectory.listFiles()))
                    .filter(filePointer -> filePointer.getName().endsWith(".jar")).toArray(File[]::new);

            File[] libJars = Arrays.stream(Objects.requireNonNull(libsFileDirectory.listFiles()))
                    .filter(filePointer -> filePointer.getName().endsWith(".jar")).toArray(File[]::new);

            List<URL> urls = new ArrayList<>(pluginJars.length + libJars.length + 1);
            for (File pluginJar : pluginJars) {
                urls.add(pluginJar.toURI().toURL());
            }
            for (File libJar : libJars) {
                urls.add(libJar.toURI().toURL());
            }
            urls.add(javaagentFile.toURI().toURL());
            return new AgentClassLoader(urls.toArray(new URL[0]), null);
        } catch (IOException | URISyntaxException e) {
            throw new AgentBootstrapFailException("failed to construct the agent classloader dur to: " + e.getMessage());
        }
    }

}

