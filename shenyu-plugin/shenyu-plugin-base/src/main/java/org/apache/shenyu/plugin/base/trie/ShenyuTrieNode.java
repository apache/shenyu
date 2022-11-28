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

import java.io.Serializable;
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
     * in path /a/b/c, b is child of a, c is child of b
     */
    private Cache<String, ShenyuTrieNode> children;

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
    private Cache<String, RuleData> pathRuleCache;

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
        this.pathRuleCache = Caffeine.newBuilder().maximumSize(size).build();
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
    public void setMatchStr(String matchStr) {
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
    public void setFullPath(String fullPath) {
        this.fullPath = fullPath;
    }

    /**
     * get trie children.
     *
     * @return trie children cache
     */
    public Cache<String, ShenyuTrieNode> getChildren() {
        return children;
    }

    /**
     * set children cache.
     *
     * @param children children
     */
    public void setChildren(Cache<String, ShenyuTrieNode> children) {
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
    public void setWildcard(boolean wildcard) {
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
    public void setEndOfPath(boolean endOfPath) {
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
    public void setBizInfo(Object bizInfo) {
        this.bizInfo = bizInfo;
    }

    /**
     * get path rule cache.
     *
     * @return rule cache
     */
    public Cache<String, RuleData> getPathRuleCache() {
        return pathRuleCache;
    }

    /**
     * set path rule cache.
     *
     * @param pathRuleCache path rule cache
     */
    public void setPathRuleCache(Cache<String, RuleData> pathRuleCache) {
        this.pathRuleCache = pathRuleCache;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ShenyuTrieNode that = (ShenyuTrieNode) o;
        return isWildcard == that.isWildcard && endOfPath == that.endOfPath && matchStr.equals(that.matchStr) && fullPath.equals(that.fullPath) && children.equals(that.children) && pathRuleCache.equals(that.pathRuleCache) && bizInfo.equals(that.bizInfo);
    }

    @Override
    public int hashCode() {
        return Objects.hash(matchStr, fullPath, children, isWildcard, endOfPath, pathRuleCache, bizInfo);
    }

    @Override
    public String toString() {
        return "ShenyuTrieNode{" +
                "matchStr='" + matchStr + '\'' +
                ", fullPath='" + fullPath + '\'' +
                ", children=" + children +
                ", isWildcard=" + isWildcard +
                ", endOfPath=" + endOfPath +
                ", pathRuleCache=" + pathRuleCache +
                ", bizInfo=" + bizInfo +
                '}';
    }
}
