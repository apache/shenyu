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

import org.apache.shenyu.common.constant.Constants;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

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
    private Map<String, ShenyuTrieNode> pathVariables;

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
    private Map<String, List<?>> pathCache;

    /**
     * biz info, if the trie is selector trie, the bizInfo is pluginName, if the trie is rule trie, the bizInfo is selectorId.
     */
    private String bizInfo;
    
    /**
     * parent node.
     */
    private ShenyuTrieNode parentNode;
    
    /**
     * fail to node.
     */
    private ShenyuTrieNode failToNode;

    public ShenyuTrieNode() {
    }

    public ShenyuTrieNode(final String matchStr, final String fullPath, final boolean endOfPath) {
        this.matchStr = matchStr;
        this.fullPath = fullPath;
        this.endOfPath = endOfPath;
        this.children = new ConcurrentHashMap<>(Constants.TRIE_CHILDREN_SIZE);
        this.pathCache = new ConcurrentHashMap<>(Constants.TRIE_PATH_CACHE_SIZE);
        this.pathVariables = new ConcurrentHashMap<>(Constants.TRIE_PATH_VARIABLES_SIZE);
        this.parentNode = null;
        this.failToNode = null;
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
    public Map<String, ShenyuTrieNode> getPathVariables() {
        return pathVariables;
    }

    /**
     * set pathVariable.
     *
     * @param pathVariablesSet pathVariablesSet
     */
    public void setPathVariables(final Map<String, ShenyuTrieNode> pathVariablesSet) {
        this.pathVariables = pathVariablesSet;
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
     * get current path biz info, the biz info maybe pluginName or selectorId.
     *
     * @return biz info
     */
    public String getBizInfo() {
        return bizInfo;
    }

    /**
     * set biz info.
     *
     * @param bizInfo bizInfo
     */
    public void setBizInfo(final String bizInfo) {
        this.bizInfo = bizInfo;
    }

    /**
     * get path cache.
     *
     * @return path cache, maybe selector or rule
     */
    public Map<String, List<?>> getPathCache() {
        return pathCache;
    }

    /**
     * set path rule cache.
     *
     * @param pathCache path cache
     */
    public void setPathRuleCache(final Map<String, List<?>> pathCache) {
        this.pathCache = pathCache;
    }
    
    /**
     * get parent node.
     *
     * @return parent node
     */
    public ShenyuTrieNode getParentNode() {
        return parentNode;
    }
    
    /**
     * set parent node.
     *
     * @param parentNode parent node
     */
    public void setParentNode(final ShenyuTrieNode parentNode) {
        this.parentNode = parentNode;
    }
    
    /**
     * get fail to node.
     *
     * @return fail to node
     */
    public ShenyuTrieNode getFailToNode() {
        return failToNode;
    }
    
    /**
     * set fail to node.
     *
     * @param failToNode fail to node
     */
    public void setFailToNode(final ShenyuTrieNode failToNode) {
        this.failToNode = failToNode;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ShenyuTrieNode)) {
            return false;
        }
        ShenyuTrieNode that = (ShenyuTrieNode) o;
        return getWildcard() == that.getWildcard() && getEndOfPath() == that.getEndOfPath() && Objects.equals(getMatchStr(), that.getMatchStr())
                && Objects.equals(getFullPath(), that.getFullPath()) && Objects.equals(getChildren(), that.getChildren())
                && Objects.equals(getPathVariables(), that.getPathVariables()) && Objects.equals(getPathVariableNode(), that.getPathVariableNode())
                && Objects.equals(getPathCache(), that.getPathCache()) && Objects.equals(getBizInfo(), that.getBizInfo())
                && Objects.equals(getParentNode(), that.getParentNode()) && Objects.equals(getFailToNode(), that.getFailToNode());
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(getMatchStr(), getFullPath(), getChildren(), getPathVariables(), getPathVariableNode(),
                getWildcard(), getEndOfPath(), getPathCache(), getBizInfo(), getParentNode(), getFailToNode());
    }
    
    @Override
    public String toString() {
        return "ShenyuTrieNode{"
                + "matchStr='" + matchStr
                + ", fullPath='" + fullPath
                + ", children=" + children
                + ", pathVariables=" + pathVariables
                + ", pathVariableNode=" + pathVariableNode
                + ", isWildcard=" + isWildcard
                + ", endOfPath=" + endOfPath
                + ", pathCache=" + pathCache
                + ", bizInfo='" + bizInfo
                + '}';
    }
}
