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

import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Objects;

/**
 * The type Shenyu plugin path builder.
 */
public final class ShenyuPluginPathBuilder {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuPluginPathBuilder.class);
    
    private static final String PLUGIN_PATH = "plugin-ext";
    
    private static File pluginPath;
    
    static {
        pluginPath = buildPluginPath();
    }
    
    /**
     * Gets plugin path.
     *
     * @return the plugin path
     */
    public static File getPluginPath() {
        return pluginPath;
    }
    
    private static File buildPluginJarPath() {
        String classResourcePath = String.join("", ShenyuPluginPathBuilder.class.getName().replaceAll("\\.", "/"), ".class");
        URL resource = ClassLoader.getSystemClassLoader().getResource(classResourcePath);
        if (null != resource) {
            String url = resource.toString();
            LOG.debug("The class location is {}.", url);
            int existFileInJarIndex = url.indexOf('!');
            boolean isInJar = existFileInJarIndex > -1;
            return isInJar ? getFileInJar(url, existFileInJarIndex) : getFileInResource(url, classResourcePath);
        }
        throw new ShenyuException("Can not locate shenyu plugin jar file.");
    }
    
    private static File getFileInResource(final String url, final String classResourcePath) {
        int prefixLength = "file:".length();
        String classLocation = url.substring(prefixLength, url.length() - classResourcePath.length());
        return new File(classLocation);
    }
    
    private static File getFileInJar(final String url, final int fileInJarIndex) {
        String realUrl = url.substring(url.indexOf("file:"), fileInJarIndex);
        try {
            File jarFile = new File(new URL(realUrl).toURI());
            return jarFile.exists() ? jarFile.getParentFile() : null;
        } catch (final MalformedURLException | URISyntaxException ex) {
            LOG.error(String.format("Can not locate shenyu plugin jar file by url %s", url), ex);
            return null;
        }
    }
    
    private static File buildPluginPath() {
        String pluginPath = System.getProperty(PLUGIN_PATH);
        if (Objects.nonNull(pluginPath)) {
            return new File(pluginPath);
        }
        return new File(String.join("", buildPluginJarPath().getPath(), "/ext-lib"));
    }
}
