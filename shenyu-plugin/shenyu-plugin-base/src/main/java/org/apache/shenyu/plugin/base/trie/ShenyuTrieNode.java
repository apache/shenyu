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

import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.dto.RuleData;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * ShenyuTrieNode.
 */
public class ShenyuTrieNode implements Serializable {

    private static final long serialVersionUID = -2347426887850566364L;

    /**
     * abc match abc, :a match all words as a variable names a, * match all words  ,** match all words and children.
     */
    private String matchStr;

    /**
     * full path.
     */
    private String fullPath;

    /**
     * in path /a/b/c, b is child of a, c is child of b.
     */
    private Map<String, ShenyuTrieNode> children;

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
     * selectorId mapping to RuleData.
     */
    private Map<String, List<RuleData>> pathRuleCache;
    
    private String selectorId;

    /**
     * biz info, route info and any other info store here, e.g. ruleId, selectorId and so on.
     */
    private Object bizInfo;

    public ShenyuTrieNode() {
    }

    public ShenyuTrieNode(final String matchStr, final String fullPath, final boolean endOfPath,
                          final Long childrenSize, final Long pathRuleCacheSize, final Long pathVariableSize) {
        this.matchStr = matchStr;
        this.fullPath = fullPath;
        this.endOfPath = endOfPath;
        this.children = new WindowTinyLFUMap<>(childrenSize);
        this.pathRuleCache = new WindowTinyLFUMap<>(pathRuleCacheSize);
        this.pathVariablesSet = new WindowTinyLFUMap<>(pathVariableSize);
    }

    /**
     * get match str.
     *
     * @return matched string
     */
    public String getMatchStr() {
        return matchStr;
    }

    /**
     * set match str.
     *
     * @param matchStr match string
     */
    public void setMatchStr(final String matchStr) {
        this.matchStr = matchStr;
    }

    /**
     * get full path.
     *
     * @return full path
     */
    public String getFullPath() {
        return fullPath;
    }

    /**
     * set full path.
     *
     * @param fullPath full path
     */
    public void setFullPath(final String fullPath) {
        this.fullPath = fullPath;
    }

    /**
     * get trie children.
     *
     * @return trie children cache
     */
    public Map<String, ShenyuTrieNode> getChildren() {
        return children;
    }

    /**
     * get pathVariable set.
     *
     * @return path variable
     */
    public Map<String, ShenyuTrieNode> getPathVariablesSet() {
        return pathVariablesSet;
    }

    /**
     * set pathVariable.
     *
     * @param pathVariablesSet pathVariablesSet
     */
    public void setPathVariablesSet(final Map<String, ShenyuTrieNode> pathVariablesSet) {
        this.pathVariablesSet = pathVariablesSet;
    }

    /**
     * get pathVariable node.
     *
     * @return ShenyuTrieNode
     */
    public ShenyuTrieNode getPathVariableNode() {
        return pathVariableNode;
    }

    /**
     * set pathVariable node.
     * @param pathVariableNode pathVariableNode
     */
    public void setPathVariableNode(final ShenyuTrieNode pathVariableNode) {
        this.pathVariableNode = pathVariableNode;
    }

    /**
     * set children cache.
     *
     * @param children children
     */
    public void setChildren(final Map<String, ShenyuTrieNode> children) {
        this.children = children;
    }

    /**
     * match all.
     *
     * @return match all will return true
     */
    public boolean getWildcard() {
        return isWildcard;
    }

    /**
     * set wildcard.
     *
     * @param wildcard wildcard
     */
    public void setWildcard(final boolean wildcard) {
        isWildcard = wildcard;
    }

    /**
     * this path is end of path.
     *
     * @return true means match success
     */
    public boolean getEndOfPath() {
        return endOfPath;
    }

    /**
     * set end of path.
     *
     * @param endOfPath end of path
     */
    public void setEndOfPath(final boolean endOfPath) {
        this.endOfPath = endOfPath;
    }

    /**
     * get current path biz info.
     *
     * @return biz info
     */
    public Object getBizInfo() {
        return bizInfo;
    }

    /**
     * set biz info.
     *
     * @param bizInfo bizInfo
     */
    public void setBizInfo(final Object bizInfo) {
        this.bizInfo = bizInfo;
    }

    /**
     * get path rule cache.
     *
     * @return rule cache
     */
    public Map<String, List<RuleData>> getPathRuleCache() {
        return pathRuleCache;
    }

    /**
     * set path rule cache.
     *
     * @param pathRuleCache path rule cache
     */
    public void setPathRuleCache(final Map<String, List<RuleData>> pathRuleCache) {
        this.pathRuleCache = pathRuleCache;
    }
    
    /**
     * get current node selector id.
     *
     * @return selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }
    
    /**
     * set current node selector id.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ShenyuTrieNode that = (ShenyuTrieNode) o;
        return isWildcard == that.isWildcard && endOfPath == that.endOfPath && matchStr.equals(that.matchStr)
                && fullPath.equals(that.fullPath) && children.equals(that.children)
                && pathVariablesSet.equals(that.pathVariablesSet) && pathVariableNode.equals(that.pathVariableNode)
                && pathRuleCache.equals(that.pathRuleCache) && bizInfo.equals(that.bizInfo)
                && selectorId.equals(that.selectorId);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(matchStr, fullPath, children, pathVariablesSet, pathVariableNode, isWildcard, endOfPath,
                pathRuleCache, bizInfo, selectorId);
    }
    
    @Override
    public String toString() {
        return "ShenyuTrieNode{"
                + "matchStr='" + matchStr + '\''
                + ", fullPath='" + fullPath + '\''
                + ", children=" + children
                + ", pathVariablesSet=" + pathVariablesSet
                + ", pathVariableNode=" + pathVariableNode
                + ", isWildcard=" + isWildcard
                + ", endOfPath=" + endOfPath
                + ", pathRuleCache=" + pathRuleCache
                + ", bizInfo=" + bizInfo
                + ", selectorId=" + selectorId
                + '}';
    }
}
