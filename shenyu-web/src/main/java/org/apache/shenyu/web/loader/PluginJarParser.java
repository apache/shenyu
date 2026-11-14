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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.config.PluginArchiveLimits;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Base64;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

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
        return parseJar(jarBytes, new PluginArchiveLimits());
    }

    /**
     * Parse every ZIP entry through the same budget before exposing a candidate.
     * ZipInputStream intentionally avoids JarInputStream's implicit manifest read.
     * @param jarBytes archive input
     * @param limits loading policy
     * @return completely parsed candidate
     */
    public static PluginJar parseJar(final byte[] jarBytes, final PluginArchiveLimits limits) {
        checkLimit(jarBytes.length, limits.getMaxArchiveBytes(), "archive input");
        PluginJar pluginJar = new PluginJar();
        long expanded = 0;
        int entries = 0;
        try (ZipInputStream input = new ZipInputStream(new ByteArrayInputStream(jarBytes))) {
            ZipEntry entry;
            while (Objects.nonNull(entry = input.getNextEntry())) {
                checkLimit(++entries, limits.getMaxEntries(), "entry count");
                int remaining = (int) (limits.getMaxExpandedBytes() - expanded);
                int allowance = Math.min(limits.getMaxEntryBytes(), remaining);
                byte[] content = readBounded(input, allowance,
                        remaining < limits.getMaxEntryBytes() ? "expanded total" : "expanded entry");
                expanded += content.length;
                String entryName = entry.getName();
                if (entryName.endsWith("pom.properties")) {
                    Properties properties = new Properties();
                    properties.load(new ByteArrayInputStream(content));
                    pluginJar.version = properties.getProperty("version");
                    pluginJar.artifactId = properties.getProperty("artifactId");
                    pluginJar.groupId = properties.getProperty("groupId");
                } else if (!entry.isDirectory() && entryName.endsWith(".class")) {
                    String className = entryName.substring(0, entryName.length() - 6).replace('/', '.');
                    pluginJar.clazzMap.put(className, content);
                } else {
                    pluginJar.resourceMap.put(entryName, content);
                }
            }
        } catch (IOException e) {
            throw new ShenyuException("load jar classes find error", e);
        }
        if (StringUtils.isAnyBlank(pluginJar.version, pluginJar.artifactId, pluginJar.groupId)) {
            throw new ShenyuException("plugin jar is missing required Maven metadata: groupId, artifactId and version");
        }
        return pluginJar;
    }

    /**
     * Read a file with an enforced input bound, including files that grow during reading.
     * @param path archive path
     * @param limits loading policy
     * @return parsed candidate
     * @throws IOException when reading fails
     */
    public static PluginJar parseFile(final Path path, final PluginArchiveLimits limits) throws IOException {
        try (InputStream input = Files.newInputStream(path)) {
            return parseJar(readBounded(input, limits.getMaxArchiveBytes(), "archive input"), limits);
        }
    }

    /**
     * Reject oversized encoded input before allocating its decoded archive.
     * @param encoded basic Base64 archive from Admin
     * @param limits loading policy
     * @return parsed candidate
     */
    public static PluginJar parseEncodedJar(final String encoded, final PluginArchiveLimits limits) {
        long length = encoded.length();
        long decodedLength = length / 4 * 3 + Math.max(0, length % 4 - 1);
        if (length % 4 == 0 && encoded.endsWith("=")) {
            decodedLength--;
            if (encoded.endsWith("==")) {
                decodedLength--;
            }
        }
        checkLimit(decodedLength, limits.getMaxArchiveBytes(), "archive input");
        return parseJar(Base64.getDecoder().decode(encoded), limits);
    }

    private static byte[] readBounded(final InputStream input, final int limit, final String dimension) throws IOException {
        try (BoundedBuffer output = new BoundedBuffer(limit)) {
            byte[] chunk = new byte[8192];
            int count;
            while ((count = input.read(chunk, 0, (int) Math.min(chunk.length, (long) limit - output.size() + 1))) != -1) {
                checkLimit((long) output.size() + count, limit, dimension);
                output.write(chunk, 0, count);
            }
            return output.toByteArray();
        }
    }

    private static void checkLimit(final long actual, final int limit, final String dimension) {
        if (actual > limit) {
            throw new ShenyuException("plugin archive exceeds " + dimension + " limit (" + limit + ")");
        }
    }

    private static final class BoundedBuffer extends ByteArrayOutputStream {

        private final int limit;

        private BoundedBuffer(final int limit) {
            super(Math.min(8192, limit));
            this.limit = limit;
        }

        @Override
        public synchronized void write(final byte[] bytes, final int offset, final int length) {
            int required = count + length;
            if (required > buf.length) {
                int capacity = (int) Math.min(limit, Math.max((long) required, (long) buf.length * 2));
                buf = Arrays.copyOf(buf, capacity);
            }
            System.arraycopy(bytes, offset, buf, count, length);
            count = required;
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
