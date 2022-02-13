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

package org.apache.shenyu.agent.core.locator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Objects;

/**
 * The type Shenyu agent locator.
 */
public final class ShenyuAgentLocator {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuAgentLocator.class);
    
    private ShenyuAgentLocator() {
    }
    
    /**
     * Locator agent file.
     *
     * @return the file
     */
    public static File locatorAgent() {
        String classResourcePath = String.join("", ShenyuAgentLocator.class.getName().replaceAll("\\.", "/"), ".class");
        URL resource = ClassLoader.getSystemClassLoader().getResource(classResourcePath);
        assert resource != null;
        String url = resource.toString();
        int existFileInJarIndex = url.indexOf('!');
        boolean isInJar = existFileInJarIndex > -1;
        return isInJar ? getFileInJar(url, existFileInJarIndex) : getFileInResource(url, classResourcePath);
    }
    
    /**
     * Locator plugin file.
     *
     * @return the file
     */
    public static File locatorPlugin() {
        final File file = locatorAgent();
        if (Objects.isNull(file)) {
            LOG.error("[shenyu agent exception] locator plugin load error. the locator agent is not found");
            throw new RuntimeException("locator plugin load error. the locator agent is not found");
        }
        return new File(String.join("", file.getPath(), "/plugins"));
    }
    
    /**
     * Locator conf file.
     *
     * @param fileName the file name
     * @return the file
     */
    public static File locatorConf(final String fileName) {
        final File file = locatorAgent();
        if (Objects.isNull(file)) {
            LOG.error("[shenyu agent exception] the locator agent is not found");
            throw new RuntimeException("[shenyu agent exception] the locator agent is not found");
        }
        return new File(String.join("/", file.getPath(), "conf", fileName));
    }
    
    private static File getFileInResource(final String url, final String classResourcePath) {
        int prefixLength = "file:".length();
        String classLocation = url.substring(prefixLength, url.length() - classResourcePath.length());
        return new File(classLocation);
    }
    
    private static File getFileInJar(final String url, final int fileInJarIndex) {
        String realUrl = url.substring(url.indexOf("file:"), fileInJarIndex);
        try {
            File agentJarFile = new File(new URL(realUrl).toURI());
            return agentJarFile.exists() ? agentJarFile.getParentFile() : null;
        } catch (final MalformedURLException | URISyntaxException ex) {
            return null;
        }
    }
}
