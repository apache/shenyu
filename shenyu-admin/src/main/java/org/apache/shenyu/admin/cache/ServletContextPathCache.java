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

package org.apache.shenyu.admin.cache;

import com.google.common.collect.Maps;

import java.util.concurrent.ConcurrentMap;

public final class ServletContextPathCache {

    private static final ServletContextPathCache INSTANCE = new ServletContextPathCache();

    /**
     * contextPath -> servletContextPath.
     */
    private static final ConcurrentMap<String, String> PATH_MAP = Maps.newConcurrentMap();

    private ServletContextPathCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ServletContextPathCache getInstance() {
        return INSTANCE;
    }

    /**
     * Cache auth data.
     *
     * @param selectorName       the selectorName
     * @param servletContextPath the servletContextPath
     */
    public void cachePath(final String selectorName, final String servletContextPath) {
        PATH_MAP.put(selectorName, servletContextPath);
    }

    /**
     * Remove contextPath.
     *
     * @param contentPath the selectorName
     */
    public void removePath(final String contentPath) {
        PATH_MAP.remove(contentPath);
    }

    /**
     * Obtain ServletContextPath.
     *
     * @param contentPath the contentPath
     * @return the servletContextPath
     */
    public String obtainServletContextPath(final String contentPath) {
        return PATH_MAP.get(contentPath);
    }
}
