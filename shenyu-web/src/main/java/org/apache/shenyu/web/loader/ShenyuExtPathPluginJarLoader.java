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

package org.apache.shenyu.web.loader;

import com.google.common.collect.Sets;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

public class ShenyuExtPathPluginJarLoader {

    private static volatile Set<String> pluginJarName = new HashSet<>();

    /**
     * Load extend plugins list.
     *
     * @param path the path
     * @return the list
     * @throws IOException the io exception
     */
    public static synchronized List<PluginJarParser.PluginJar> loadExtendPlugins(final String path) throws IOException {
        File[] jarFiles = ShenyuPluginPathBuilder.getPluginFile(path).listFiles(file -> file.getName().endsWith(".jar"));
        if (Objects.isNull(jarFiles)) {
            return Collections.emptyList();
        }
        List<PluginJarParser.PluginJar> uploadPluginJars = new ArrayList<>();
        Set<String> currentPaths = new HashSet<>();
        for (File file : jarFiles) {
            String absolutePath = file.getAbsolutePath();
            currentPaths.add(absolutePath);
            if (pluginJarName.contains(absolutePath)) {
                continue;
            }
            byte[] pluginBytes = Files.readAllBytes(Paths.get(absolutePath));
            PluginJarParser.PluginJar uploadPluginJar = PluginJarParser.parseJar(pluginBytes);
            uploadPluginJar.setAbsolutePath(absolutePath);
            uploadPluginJars.add(uploadPluginJar);
        }
        Sets.SetView<String> removePluginSet = Sets.difference(pluginJarName, currentPaths);
        for (String removePath : removePluginSet) {
            ShenyuPluginClassLoaderHolder.getSingleton().removePluginClassLoader(removePath);
        }
        pluginJarName = currentPaths;
        return uploadPluginJars;
    }

}
