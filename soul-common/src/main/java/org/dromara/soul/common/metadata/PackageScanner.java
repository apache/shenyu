/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.common.metadata;

import java.io.IOException;
import java.net.URL;
import java.util.Set;

/**
 * PackageScanner
 * Scan the implementation classes under a package.
 *
 * @author sixh
 */
public interface PackageScanner {

    /**
     * Gets all Class information.
     *
     * @return list. full class
     * @throws IOException the io exception
     */
    Set<Resource> getFullClass() throws IOException;

    /**
     * To splash string.
     *
     * @param basePackage the base package
     * @return the string.
     */
    static String toSplash(String basePackage) {
        String s = basePackage.replaceAll("\\.", "/");
        if (!s.endsWith("/")) {
            s += "/";
        }
        return s;
    }

    /**
     * Gets root path.
     *
     * @param url the url
     * @return the root path.
     */
    static String getRootPath(URL url) {
        String fileUrl = url.getFile();
        int pos = fileUrl.indexOf('!');

        if (-1 == pos) {
            return fileUrl;
        }
        return fileUrl.substring(5, pos);
    }
}
