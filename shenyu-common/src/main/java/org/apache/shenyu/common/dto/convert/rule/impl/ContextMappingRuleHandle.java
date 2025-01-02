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
public class ContextMappingRuleHandle implements RuleHandle {

    private boolean addPrefixed;

    private String contextPath;

    private String addPrefix;
    
    private String rewriteContextPath;
    
    /**
     * percentage of rewritten traffic in context.
     */
    private Integer percentage;
    
    /**
     * New instance context mapping rule handle.
     *
     * @return the context mapping rule handle
     */
    public static ContextMappingRuleHandle newInstance() {
        return new ContextMappingRuleHandle();
    }
    
    /**
     * get prefix forward status.
     *
     * @return prefix -forward status
     */
    public boolean getAddPrefixed() {
        return addPrefixed;
    }
    
    /**
     * set prefix forward.
     *
     * @param addPrefixed status
     */
    public void setAddPrefixed(final boolean addPrefixed) {
        this.addPrefixed = addPrefixed;
    }
    
    /**
     * get contextPath.
     *
     * @return contextPath context path
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
     * @return addPrefix add prefix
     */
    public String getAddPrefix() {
        return addPrefix;
    }
    
    /**
     * set addPrefix.
     *
     * @param addPrefix addPrefix
     */
    public void setAddPrefix(final String addPrefix) {
        this.addPrefix = addPrefix;
    }
    
    /**
     * get rewrite context path.
     *
     * @return rewrite context path
     */
    public String getRewriteContextPath() {
        return rewriteContextPath;
    }
    
    /**
     * set rewrite context path.
     *
     * @param rewriteContextPath rewrite context path
     */
    public void setRewriteContextPath(final String rewriteContextPath) {
        this.rewriteContextPath = rewriteContextPath;
    }
    
    /**
     * get percentage.
     *
     * @return percentage
     */
    public Integer getPercentage() {
        return percentage;
    }
    
    /**
     * set percentage.
     *
     * @param percentage percentage
     */
    public void setPercentage(final Integer percentage) {
        this.percentage = percentage;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        ContextMappingRuleHandle that = (ContextMappingRuleHandle) o;
        return Objects.equals(contextPath, that.contextPath) && Objects.equals(addPrefix, that.addPrefix)
                && Objects.equals(addPrefixed, that.addPrefixed) && Objects.equals(rewriteContextPath, that.rewriteContextPath);
    }

    @Override
    public String toString() {
        return "ContextMappingRuleHandle{"
                + "contextPath='"
                + contextPath
                + '\''
                + ", addPrefix='"
                + addPrefix
                + '\''
                + "addPrefixed='"
                + addPrefixed
                + '\''
                + ", rewriteContextPath='"
                + rewriteContextPath
                + '\''
                + ", percentage='"
                + percentage
                + '\''
                + '}';
    }

    @Override
    public int hashCode() {
        return Objects.hash(contextPath, addPrefix, addPrefixed, rewriteContextPath);
    }
}
