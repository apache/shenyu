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
    
    private static final String PRE_FILE = "file:";
    
    private ShenyuAgentLocator() {
    }
    
    /**
     * Locator agent file.
     *
     * @return the file
     */
    public static File locatorAgent() {
        String path = String.join("", ShenyuAgentLocator.class.getName().replaceAll("\\.", "/"), ".class");
        URL resource = ClassLoader.getSystemClassLoader().getResource(path);
        assert resource != null;
        String url = resource.toString();
        int index = url.indexOf('!');
        return index > -1 ? buildFileInJar(url, index) : buildFileInResource(url, path);
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
    
    private static File buildFileInResource(final String url, final String path) {
        return new File(url.substring(PRE_FILE.length(), url.length() - path.length()));
    }
    
    private static File buildFileInJar(final String url, final int index) {
        try {
            final File jarFile = new File(new URL(url.substring(url.indexOf(PRE_FILE), index)).toURI());
            if (jarFile.exists()) {
                return jarFile.getParentFile();
            }
            return null;
        } catch (final MalformedURLException | URISyntaxException ex) {
            return null;
        }
    }
}
