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

package org.apache.shenyu.common.dto.convert.rule.impl;

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;

import java.util.Objects;

/**
 * Context mapping thread handle.
 */
public class ContextMappingHandle implements RuleHandle {

    private static final long serialVersionUID = 4891655505357494670L;

    private String contextPath;

    private String addPrefix;

    /**
     * get contextPath.
     *
     * @return contextPath
     */
    public String getContextPath() {
        return contextPath;
    }

    /**
     * set contextPath.
     *
     * @param contextPath contextPath
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }

    /**
     * get addPrefix.
     *
     * @return addPrefix
     */
    public String getAddPrefix() {
        return addPrefix;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ContextMappingHandle that = (ContextMappingHandle) o;
        return Objects.equals(contextPath, that.contextPath) && Objects.equals(addPrefix, that.addPrefix);
    }

    @Override
    public String toString() {
        return "ContextMappingHandle{"
                + "contextPath='"
                + contextPath
                + '\''
                + ", addPrefix='"
                + addPrefix
                + '\''
                + '}';
    }

    @Override
    public int hashCode() {
        return Objects.hash(contextPath, addPrefix);
    }

    /**
     * set addPrefix.
     *
     * @param addPrefix addPrefix
     */
    public void setAddPrefix(final String addPrefix) {
        this.addPrefix = addPrefix;
    }

    @Override
    public RuleHandle createDefault(final String path) {
        this.contextPath = path;
        return this;
    }
}
