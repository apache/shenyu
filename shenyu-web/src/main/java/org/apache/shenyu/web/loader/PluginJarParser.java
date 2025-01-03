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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;

/**
 * PluginJarParser.
 */
public class PluginJarParser {

    /**
     * parseJar.
     *
     * @param jarBytes jarBytes
     * @return PluginJar
     */
    public static PluginJar parseJar(final byte[] jarBytes) {
        PluginJar pluginJar = new PluginJar();
        try (JarInputStream jarInputStream = new JarInputStream(new ByteArrayInputStream(jarBytes))) {
            JarEntry jarEntry;
            while (Objects.nonNull(jarEntry = jarInputStream.getNextJarEntry())) {
                String entryName = jarEntry.getName();
                // get jar version
                if (jarEntry.getName().endsWith("pom.properties")) {
                    try (ByteArrayOutputStream buffer = new ByteArrayOutputStream()) {
                        int data;
                        while ((data = jarInputStream.read()) != -1) {
                            buffer.write(data);
                        }
                        buffer.flush();
                        byte[] classByteArray = buffer.toByteArray();
                        Properties properties = new Properties();
                        properties.load(new ByteArrayInputStream(classByteArray));
                        pluginJar.version = properties.get("version").toString();
                        pluginJar.artifactId = properties.get("artifactId").toString();
                        pluginJar.groupId = properties.get("groupId").toString();
                    }
                    continue;
                }
                if (!jarEntry.isDirectory() && entryName.endsWith(".class") && !entryName.contains("$")) {
                    String className = jarEntry.getName().substring(0, entryName.length() - 6).replaceAll("/", ".");
                    pluginJar.clazzMap.put(className, getClassByteArray(jarInputStream));
                } else {
                    pluginJar.resourceMap.put(jarEntry.getName(), getClassByteArray(jarInputStream));
                }
            }
        } catch (IOException e) {
            throw new ShenyuException("load jar classes find error");
        }
        return pluginJar;
    }

    /**
     * getClassByteArray.
     * @param jarInputStream jarInputStream
     * @return class byte[]
     */
    private static byte[] getClassByteArray(final JarInputStream jarInputStream) {
        try (ByteArrayOutputStream buffer = new ByteArrayOutputStream()) {
            int data;
            while ((data = jarInputStream.read()) != -1) {
                buffer.write(data);
            }
            buffer.flush();
            return buffer.toByteArray();
        } catch (IOException e) {
            throw new ShenyuException("load jar classes find error");
        }
    }

    public static class PluginJar {

        private String absolutePath;

        private String groupId;

        private String artifactId;

        private String version;

        private Map<String, byte[]> clazzMap = new ConcurrentHashMap<>();

        private Map<String, byte[]> resourceMap = new ConcurrentHashMap<>();

        /**
         * getAbsolutePath.
         *
         * @return absolutePath
         */
        public String getAbsolutePath() {
            return absolutePath;
        }

        /**
         * setAbsolutePath.
         *
         * @param absolutePath absolutePath
         */
        public void setAbsolutePath(final String absolutePath) {
            this.absolutePath = absolutePath;
        }

        /**
         * getGroupId.
         *
         * @return groupId
         */
        public String getGroupId() {
            return groupId;
        }

        /**
         * setGroupId.
         *
         * @param groupId groupId
         */
        public void setGroupId(final String groupId) {
            this.groupId = groupId;
        }

        /**
         * getArtifactId.
         *
         * @return artifactId
         */
        public String getArtifactId() {
            return artifactId;
        }

        /**
         * setArtifactId.
         *
         * @param artifactId artifactId.
         */
        public void setArtifactId(final String artifactId) {
            this.artifactId = artifactId;
        }

        /**
         * getVersion.
         *
         * @return version
         */
        public String getVersion() {
            return version;
        }

        /**
         * setVersion.
         *
         * @param version version
         */
        public void setVersion(final String version) {
            this.version = version;
        }

        /**
         * getClazzMap.
         *
         * @return clazzMap
         */
        public Map<String, byte[]> getClazzMap() {
            return clazzMap;
        }

        /**
         * setClazzMap.
         *
         * @param clazzMap clazzMap
         */
        public void setClazzMap(final Map<String, byte[]> clazzMap) {
            this.clazzMap = clazzMap;
        }

        /**
         * getJarKey.
         *
         * @return jarKey
         */
        public String getJarKey() {
            return String.format("%s:%s", groupId, artifactId);
        }

        /**
         * getResourceMap.
         * @return resource byte
         */
        public Map<String, byte[]> getResourceMap() {
            return resourceMap;
        }

        /**
         * setResourceMap.
         * @param resourceMap resourceMap.
         */
        public void setResourceMap(final Map<String, byte[]> resourceMap) {
            this.resourceMap = resourceMap;
        }
    }

}
