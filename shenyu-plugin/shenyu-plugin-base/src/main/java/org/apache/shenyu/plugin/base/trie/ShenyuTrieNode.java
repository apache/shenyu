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

package org.apache.shenyu.plugin.base.trie;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.apache.shenyu.common.dto.RuleData;

import java.util.Map;
import java.util.Objects;

/**
 * ShenyuTrieNode.
 */
public class ShenyuTrieNode {

    /**
     * abc match abc, :a match all words as a variable names a, * match all words  ,** match all words and children.
     */
    private String matchStr;

    /**
     * full path.
     */
    private String fullPath;

    /**
     * in path /a/b/c, b is child of a, c is child of b
     */
    private Cache<String, ShenyuTrieNode> children;

    /**
     * path variables.
     */
    private Map<String, ShenyuTrieNode> pathVariablesSet;

    /**
     * path variable node.
     */
    private ShenyuTrieNode pathVariableNode;

    /**
     * isWildcard, match all nodes, /a/b/**  /** is a match all Node.
     */
    private boolean isWildcard;

    /**
     * if true means a real path exists, /a/b/c/d only node of d is true, a,b,c is false.
     */
    private boolean endOfPath;

    /**
     * selectorId -> RuleData
     */
    private Cache<String, RuleData> pluginRuleMap;

    /**
     * biz info, route info and any other info store here, e.g. ruleId, selectorId and so on.
     */
    private Object bizInfo;

    public ShenyuTrieNode() {
    }

    public ShenyuTrieNode(final String matchStr, final String fullPath, final boolean endOfPath, final Long size) {
        this.matchStr = matchStr;
        this.fullPath = fullPath;
        this.endOfPath = endOfPath;
        this.pluginRuleMap = Caffeine.newBuilder().maximumSize(size).build();
    }

    public String getMatchStr() {
        return matchStr;
    }

    public void setMatchStr(String matchStr) {
        this.matchStr = matchStr;
    }

    public String getFullPath() {
        return fullPath;
    }

    public void setFullPath(String fullPath) {
        this.fullPath = fullPath;
    }

    public Cache<String, ShenyuTrieNode> getChildren() {
        return children;
    }

    public void setChildren(Cache<String, ShenyuTrieNode> children) {
        this.children = children;
    }

    public Map<String, ShenyuTrieNode> getPathVariablesSet() {
        return pathVariablesSet;
    }

    public void setPathVariablesSet(Map<String, ShenyuTrieNode> pathVariablesSet) {
        this.pathVariablesSet = pathVariablesSet;
    }

    public ShenyuTrieNode getPathVariableNode() {
        return pathVariableNode;
    }

    public void setPathVariableNode(ShenyuTrieNode pathVariableNode) {
        this.pathVariableNode = pathVariableNode;
    }

    public boolean getWildcard() {
        return isWildcard;
    }

    public void setWildcard(boolean wildcard) {
        isWildcard = wildcard;
    }

    public boolean getEndOfPath() {
        return endOfPath;
    }

    public void setEndOfPath(boolean endOfPath) {
        this.endOfPath = endOfPath;
    }

    public Object getBizInfo() {
        return bizInfo;
    }

    public void setBizInfo(Object bizInfo) {
        this.bizInfo = bizInfo;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ShenyuTrieNode that = (ShenyuTrieNode) o;
        return isWildcard == that.isWildcard && endOfPath == that.endOfPath && matchStr.equals(that.matchStr) && fullPath.equals(that.fullPath) && children.equals(that.children) && pathVariablesSet.equals(that.pathVariablesSet) && pathVariableNode.equals(that.pathVariableNode) && bizInfo.equals(that.bizInfo);
    }

    public Cache<String, RuleData> getPluginRuleMap() {
        return pluginRuleMap;
    }

    public void setPluginRuleMap(Cache<String, RuleData> pluginRuleMap) {
        this.pluginRuleMap = pluginRuleMap;
    }

    @Override
    public int hashCode() {
        return Objects.hash(matchStr, fullPath, children, pathVariablesSet, pathVariableNode, isWildcard, endOfPath, bizInfo);
    }

    @Override
    public String toString() {
        return "ShenyuTrieNode{" +
                "matchStr='" + matchStr + '\'' +
                ", fullPath='" + fullPath + '\'' +
                ", children=" + children +
                ", pathVariablesSet=" + pathVariablesSet +
                ", pathVariableNode=" + pathVariableNode +
                ", isWildcard=" + isWildcard +
                ", endOfPath=" + endOfPath +
                ", pluginRuleMap=" + pluginRuleMap +
                ", bizInfo=" + bizInfo +
                '}';
    }
}
