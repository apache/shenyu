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

package org.apache.shenyu.admin.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;

/**
 * Namespace utility class.
 */
public final class NamespaceUtils {

    private NamespaceUtils() {
    }

    /**
     * Normalize namespace ID.
     * If the namespaceId is blank or equals "default" (case-insensitive),
     * returns the system default namespace ID.
     *
     * @param namespaceId namespace ID
     * @return normalized namespace ID
     */
    public static String normalizeNamespace(final String namespaceId) {
        if (StringUtils.isBlank(namespaceId)
                || StringUtils.equalsIgnoreCase(namespaceId, "default")) {
            return Constants.SYS_DEFAULT_NAMESPACE_ID;
        }
        return namespaceId;
    }
}

