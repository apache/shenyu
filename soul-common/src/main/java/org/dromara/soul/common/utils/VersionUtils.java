/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.common.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.security.CodeSource;

/**
 * VersionUtils.
 *
 * @author xiaoyu(Myth)
 */
public class VersionUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(VersionUtils.class);

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
        try {
            // find version info from MANIFEST.MF first
            String version = cls.getPackage().getImplementationVersion();
            if (version == null || version.length() == 0) {
                version = cls.getPackage().getSpecificationVersion();
            }
            if (version == null || version.length() == 0) {
                // guess version fro jar file name if nothing's found from MANIFEST.MF
                CodeSource codeSource = cls.getProtectionDomain().getCodeSource();
                if (codeSource == null) {
                    LOGGER.info("No codeSource for class " + cls.getName() + " when getVersion, use default version " + defaultVersion);
                } else {
                    String file = codeSource.getLocation().getFile();
                    if (file != null && file.length() > 0 && file.endsWith(JAR)) {
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
                            if (i >= 0) {
                                file = file.substring(i + 1);
                            } else {
                                break;
                            }
                        }
                        version = file;
                    }
                }
            }
            // return default version if no version info is found
            return version == null || version.length() == 0 ? defaultVersion : version;
        } catch (Throwable e) {
            // return default version when any exception is thrown
            LOGGER.error("return default version, ignore exception " + e.getMessage(), e);
            return defaultVersion;
        }
    }

}
