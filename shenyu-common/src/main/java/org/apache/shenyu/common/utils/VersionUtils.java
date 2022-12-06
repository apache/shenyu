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

package org.apache.shenyu.common.utils;

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.security.CodeSource;

/**
 * VersionUtils.
 */
public final class VersionUtils {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(VersionUtils.class);

    private static final String VERSION = getVersion(VersionUtils.class, "1.0.0");

    private static final String JAR = ".jar";

    private VersionUtils() {
    }

    /**
     * Gets version.
     *
     * @return the version
     */
    public static String getVersion() {
        return VERSION;
    }

    /**
     * Gets version.
     *
     * @param cls            the cls
     * @param defaultVersion the default version
     * @return the version
     */
    public static String getVersion(final Class<?> cls, final String defaultVersion) {
        // find version info from MANIFEST.MF first
        String version = cls.getPackage().getImplementationVersion();
        if (StringUtils.isBlank(version)) {
            version = cls.getPackage().getSpecificationVersion();
        }
        if (StringUtils.isNoneBlank(version)) {
            return version;
        }
        // guess version for jar file name if nothing's found from MANIFEST.MF
        CodeSource codeSource = cls.getProtectionDomain().getCodeSource();

        if (codeSource == null) {
            LOG.info("No codeSource for class {} when getVersion, use default version {}", cls.getName(), defaultVersion);
            return defaultVersion;
        }
        String file = codeSource.getLocation().getFile();
        if (file != null && file.endsWith(JAR)) {
            file = file.substring(0, file.length() - 4);
            int i = file.lastIndexOf('/');
            if (i >= 0) {
                file = file.substring(i + 1);
            }
            i = file.indexOf("-");
            if (i >= 0) {
                file = file.substring(i + 1);
            }
            while (file.length() > 0 && !Character.isDigit(file.charAt(0))) {
                i = file.indexOf("-");
                if (i < 0) {
                    break;
                }
                file = file.substring(i + 1);
            }
            version = file;
        }
        // return default version if no version info is found
        return StringUtils.isBlank(version) ? defaultVersion : version;
    }

    /**
     * checkDuplicate,this method refers to the design of dubbo,url:https://dubbo.apache.org/zh/docs/v2.7/dev/principals/dummy/ .
     * @param cls cls
     */
    public static void checkDuplicate(final Class<?> cls) {
        try {
            String path = cls.getName().replace('.', '/') + ".class";
            Set<String> files = readResources(path, cls);
            // duplicated jar is found
            if (files.size() > 1) {
                String error = "Duplicate class " + path + " in " + files.size() + " jar " + files;
                LOG.error("checkDuplicate error,{}", error);
            }
        } catch (Throwable e) {
            LOG.error("checkDuplicate error,msg={},e={}", e.getMessage(), e);
        }
    }

    /**
     * readResources.
     * @param path path
     * @param cls cls
     * @return set
     * @throws IOException ioexception
     */
    private static Set<String> readResources(final String path, final Class<?> cls) throws IOException {
        Enumeration<URL> urls = cls.getClassLoader().getResources(path);
        Set<String> files = new HashSet<String>();
        while (urls.hasMoreElements()) {
            URL url = urls.nextElement();
            if (url != null) {
                String file = url.getFile();
                if (StringUtils.isNotEmpty(file)) {
                    files.add(file);
                }
            }
        }
        return files;
    }
}


