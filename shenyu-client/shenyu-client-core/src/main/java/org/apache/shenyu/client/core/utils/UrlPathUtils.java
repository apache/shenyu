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

package org.apache.shenyu.client.core.utils;

import org.apache.commons.lang3.StringUtils;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * UrlPathUtils.
 */
public class UrlPathUtils {

    public static final String SLASH = "/";

    /**
     * getSegments.
     * @param path path
     * @return segments
     */
    public static List<String> getSegments(final String path) {
        String fixPath = fixPath(path);
        if (StringUtils.isNotEmpty(fixPath)) {
            return Arrays.asList(StringUtils.split(fixPath, SLASH));
        }
        return Collections.emptyList();
    }

    /**
     * remove '/' and namespace .
     * @param path path
     * @return fixpath
     */
    public static String fixPath(final String path) {
        if (StringUtils.equals(SLASH, path)) {
            return StringUtils.EMPTY;
        }
        String segmentStr = StringUtils.trim(path);
        segmentStr = StringUtils.removeStart(segmentStr, SLASH);
        segmentStr = StringUtils.removeEnd(segmentStr, SLASH);
        segmentStr = StringUtils.trim(segmentStr);
        return segmentStr;
    }

}
