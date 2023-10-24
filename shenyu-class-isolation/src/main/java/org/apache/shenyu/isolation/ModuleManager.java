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

package org.apache.shenyu.isolation;

import java.io.File;
import java.io.FilenameFilter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Objects;

public class ModuleManager {

    /**
     * Init classloader.
     * @param dir File dir.
     * @return Plugin classloader.
     * @throws MalformedURLException Exception.
     */
    public static URLClassLoader initClassLoader(final File dir) throws MalformedURLException {
        File[] jars = dir.listFiles((dir1, name) -> name.endsWith(".jar"));
        if (Objects.isNull(jars) || jars.length == 0) {
            return null;
        }

        URL[] classPath = new URL[jars.length + 1];
        classPath[0] = dir.toURI().toURL();

        for (int i = 1; i < classPath.length; i++) {
            classPath[i] = jars[i - 1].toURI().toURL();
        }
        return new URLClassLoader(classPath, ModuleManager.class.getClassLoader());
    }
}
