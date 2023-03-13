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
import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.TrieMatchModeEvent;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

public class ShenyuTrie {

    private static final String WILDCARD = "*";

    private static final String MATCH_ALL = "**";

    private final ShenyuTrieNode root;

    private final Long childrenSize;

    private final Long pathRuleCacheSize;
    
    private final Long pathVariableSize;

    /**
     * the mode includes antPathMatch and pathPattern, please see {@linkplain TrieMatchModeEvent}.
     * antPathMatch means all full match, pathPattern is used in web.
     */
    private final String matchMode;

    public ShenyuTrie(final Long childrenSize, final Long pathRuleCacheSize, final Long pathVariableSize, final String matchMode) {
        this.root = new ShenyuTrieNode("/", "/", false, childrenSize, pathRuleCacheSize, pathVariableSize);
        this.childrenSize = childrenSize;
        this.pathRuleCacheSize = pathRuleCacheSize;
        this.pathVariableSize = pathVariableSize;
        this.matchMode = matchMode;
    }

    /**
     * clear the trie.
     */
    public void clear() {
        cleanup(this.root.getChildren());
        cleanup(this.root.getPathRuleCache());
        cleanup(this.root.getPathVariablesSet());
        this.root.setPathVariableNode(null);
    }

    /**
     * judge the trie is empty.
     *
     * @return status
     */
    public boolean isEmpty() {
        return this.root.getChildren().estimatedSize() == 0
                && this.root.getPathVariablesSet().estimatedSize() == 0
                && Objects.isNull(this.root.getPathVariableNode());
    }

    /**
     * put node to trie.
     *
     * @param uriPaths uri path
     * @param ruleData rule data
     * @param bizInfo biz info
     */
    public void putNode(final List<String> uriPaths, final RuleData ruleData, final Object bizInfo) {
        if (CollectionUtils.isNotEmpty(uriPaths)) {
            uriPaths.forEach(path -> putNode(path, ruleData, bizInfo));
        }
    }

    /**
     * put node to trie.
     *
     * @param uriPath uri path
     * @param ruleData rule data
     * @param bizInfo biz info
     */
    public void putNode(final String uriPath, final RuleData ruleData, final Object bizInfo) {
        if (StringUtils.isNotBlank(uriPath)) {
            String strippedPath = StringUtils.strip(uriPath, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            if (pathParts.length > 0) {
                ShenyuTrieNode node = root;
                for (int i = 0; i < pathParts.length; i++) {
                    boolean endOfPath = isMatchAllOrWildcard(pathParts[i]) && judgeEqual(i, pathParts.length - 1);
                    node = putNode0(pathParts[i], node, matchMode, endOfPath);
                }
                // after insert node, set full path and end of path
                node.setFullPath(uriPath);
                node.setEndOfPath(true);
                node.setBizInfo(bizInfo);
                if (Objects.isNull(node.getPathRuleCache())) {
                    node.setPathRuleCache(Caffeine.newBuilder().maximumSize(pathRuleCacheSize).build());
                }
                List<RuleData> ruleDataList = getVal(node.getPathRuleCache(), ruleData.getSelectorId());
                if (CollectionUtils.isNotEmpty(ruleDataList)) {
                    // synchronized list
                    synchronized (ruleData.getSelectorId()) {
                        ruleDataList.add(ruleData);
                        ruleDataList.sort(Comparator.comparing(RuleData::getSort));
                        node.getPathRuleCache().put(ruleData.getSelectorId(), ruleDataList);
                    }
                } else {
                    node.getPathRuleCache().put(ruleData.getSelectorId(), Lists.newArrayList(ruleData));
                }
            }
        }
    }
    
    /**
     * put node to trie.
     *
     * @param segment current string
     * @param shenyuTrieNode current trie node
     * @param matchMode match mode
     * @param isPathEnd end path
     * @return {@linkplain ShenyuTrieNode}
     */
    private ShenyuTrieNode putNode0(final String segment, final ShenyuTrieNode shenyuTrieNode,
                                    final String matchMode, final boolean isPathEnd) {
        // if match mode is path pattern, when segment is * and **, return current node
        if (TrieMatchModeEvent.PATH_PATTERN.getMatchMode().equals(matchMode)) {
            if (isMatchAll(segment)) {
                // put node, and return node
                return this.put(segment, shenyuTrieNode, true);
            }
            if (isMatchWildcard(segment)) {
                ShenyuTrieNode wildcardNode = this.put(segment, shenyuTrieNode, true);
                wildcardNode.setWildcard(true);
                return wildcardNode;
            }
        }
        if (TrieMatchModeEvent.ANT_PATH_MATCH.getMatchMode().equals(matchMode)) {
            if (isMatchAll(segment) && isPathEnd) {
                return this.put(segment, shenyuTrieNode, true);
            }
            if (isMatchWildcard(segment) && isPathEnd) {
                ShenyuTrieNode wildcardNode = this.put(segment, shenyuTrieNode, true);
                wildcardNode.setWildcard(true);
                return wildcardNode;
            }
        }
        // dynamic route
        if (isPathVariable(segment)) {
            ShenyuTrieNode childNode;
            // contains key, get current pathVariable node
            if (containsKey(shenyuTrieNode.getPathVariablesSet(), segment)) {
                childNode = getVal(shenyuTrieNode.getPathVariablesSet(), segment);
            } else {
                childNode = new ShenyuTrieNode();
                childNode.setMatchStr(segment);
                childNode.setEndOfPath(false);
                if (Objects.isNull(shenyuTrieNode.getPathVariablesSet())) {
                    shenyuTrieNode.setPathVariablesSet(Caffeine.newBuilder().maximumSize(pathVariableSize).build());
                }
                shenyuTrieNode.getPathVariablesSet().put(segment, childNode);
                shenyuTrieNode.setPathVariableNode(childNode);
            }
            return childNode;
        }
        return this.put(segment, shenyuTrieNode, false);
    }
    
    /**
     * put node.
     *
     * @param segment segment
     * @param shenyuTrieNode shenyu trie node
     * @param endOfPath end of path
     * @return ShenyuTrieNode
     */
    private ShenyuTrieNode put(final String segment, final ShenyuTrieNode shenyuTrieNode, final boolean endOfPath) {
        if (Objects.isNull(shenyuTrieNode.getChildren())) {
            shenyuTrieNode.setChildren(Caffeine.newBuilder().maximumSize(childrenSize).build());
        }
        ShenyuTrieNode childrenNode;
        if (containsKey(shenyuTrieNode.getChildren(), segment)) {
            childrenNode = getVal(shenyuTrieNode.getChildren(), segment);
        } else {
            childrenNode = new ShenyuTrieNode();
            childrenNode.setMatchStr(segment);
            childrenNode.setEndOfPath(endOfPath);
            shenyuTrieNode.getChildren().put(segment, childrenNode);
        }
        return childrenNode;
    }

    /**
     * match trie, trie exist and match the path will return current node.
     *
     * @param uriPath uri path
     * @param selectorId selectorId
     * @return {@linkplain ShenyuTrieNode}
     */
    public ShenyuTrieNode match(final String uriPath, final String selectorId) {
        Objects.requireNonNull(selectorId);
        if (!StringUtils.isEmpty(uriPath)) {
            String strippedPath = StringUtils.strip(uriPath, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            if (pathParts.length > 0) {
                ShenyuTrieNode currentNode = root;
                for (int i = 0; i < pathParts.length; i++) {
                    String path = pathParts[i];
                    currentNode = matchNode(path, currentNode);
                    if (Objects.nonNull(currentNode)) {
                        boolean endPath = judgeEqual(i, pathParts.length - 1);
                        // path is not end, continue to execute
                        if (checkChildrenNotNull(currentNode) && !currentNode.getEndOfPath()) {
                            continue;
                        }
                        // include path variable node, general node, wildcard node
                        if ((endPath || (isMatchAll(currentNode.getMatchStr()) && currentNode.getEndOfPath()))
                                && checkPathRuleNotNull(currentNode)
                                && CollectionUtils.isNotEmpty(getVal(currentNode.getPathRuleCache(), selectorId))) {
                            return currentNode;
                        }
                    } else {
                        return null;
                    }
                }
            }
        }
        return null;
    }
    
    /**
     * match node.
     * <p> priority: path > * > ** > pathVariableNode </p>
     *
     * @param segment path segment
     * @param node node
     * @return {@linkplain ShenyuTrieNode}
     */
    private ShenyuTrieNode matchNode(final String segment, final ShenyuTrieNode node) {
        if (Objects.nonNull(node)) {
            // node exist in children,first find path, avoid A plug have /http/**, B plug have /http/order/**
            if (checkChildrenNotNull(node)) {
                if (containsKey(node.getChildren(), segment)) {
                    return getVal(node.getChildren(), segment);
                }
                if (containsKey(node.getChildren(), WILDCARD)) {
                    return getVal(node.getChildren(), WILDCARD);
                }
                if (containsKey(node.getChildren(), MATCH_ALL)) {
                    return getVal(node.getChildren(), MATCH_ALL);
                }
            }
            // if node is path variable node
            if (Objects.nonNull(node.getPathVariableNode())) {
                return node.getPathVariableNode();
            }
        }
        return null;
    }

    /**
     * remove trie node.
     * remove rules: query node of the current path, if the node exists,
     * checks whether the current node is mapped to multiple plug-in rules.
     * if the plug-in rules have only on mapping, remove the node from parent.
     * if current node exists multi mappings, remove the mapping.
     *
     * @param paths paths
     * @param ruleData ruleData
     */
    public void remove(final List<String> paths, final RuleData ruleData) {
        if (CollectionUtils.isNotEmpty(paths)) {
            paths.forEach(path -> remove(path, ruleData));
        }
    }

    /**
     * remove trie node.
     * remove rules: query node of the current path, if the node exists,
     * checks whether the current node is mapped to multiple plug-in rules.
     * if the plug-in rules have only on mapping, remove the node from parent.
     * if current node exists multi mappings, remove the mapping.
     *
     * @param path path
     * @param ruleData ruleData
     */
    public void remove(final String path, final RuleData ruleData) {
        if (StringUtils.isNotBlank(path)) {
            String strippedPath = StringUtils.strip(path, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            String key = pathParts[pathParts.length - 1];
            ShenyuTrieNode currentNode = this.getNode(path);
            // node is not null, judge exist many plugin mapping
            if (Objects.nonNull(currentNode) && Objects.nonNull(currentNode.getPathRuleCache())) {
                // check current mapping
                List<RuleData> ruleDataList = getVal(currentNode.getPathRuleCache(), ruleData.getSelectorId());
                if (CollectionUtils.isNotEmpty(ruleDataList) && ruleDataList.size() == 1 && Objects.isNull(currentNode.getChildren())) {
                    // remove current node from parent node
                    String[] parentPathArray = Arrays.copyOfRange(pathParts, 0, pathParts.length - 1);
                    String parentPath = String.join("/", parentPathArray);
                    ShenyuTrieNode parentNode = this.getNode(parentPath);
                    parentNode.getChildren().invalidate(key);
                    parentNode.getChildren().cleanUp();
                } else {
                    // remove plugin mapping
                    List<RuleData> delRuleData = getVal(currentNode.getPathRuleCache(), ruleData.getSelectorId());
                    if (CollectionUtils.isNotEmpty(delRuleData)) {
                        synchronized (ruleData.getSelectorId()) {
                            delRuleData.removeIf(rule -> rule.getId().equals(ruleData.getSelectorId()));
                            currentNode.getPathRuleCache().put(ruleData.getSelectorId(), delRuleData);
                        }
                    }
                }
            }
        }
    }

    /**
     * get node from trie.
     *
     * @param uriPath uri path
     * @return {@linkplain ShenyuTrieNode}
     */
    public ShenyuTrieNode getNode(final String uriPath) {
        if (StringUtils.isNotBlank(uriPath)) {
            String strippedPath = StringUtils.strip(uriPath, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            if (pathParts.length > 0) {
                return getNode0(root, pathParts);
            }
        }
        return null;
    }

    /**
     * get node.
     *
     * @param node      node
     * @param pathParts path parts
     * @return {@linkplain ShenyuTrieNode}
     */
    private ShenyuTrieNode getNode0(final ShenyuTrieNode node, final String[] pathParts) {
        // get key in path part arrays
        String key = pathParts[0];
        String[] slice = Arrays.copyOfRange(pathParts, 1, pathParts.length);
        // if exist one path
        if (slice.length == 0) {
            if (isMatchAllOrWildcard(key)) {
                return getVal(node.getChildren(), key);
            } else if (isPathVariable(key)) {
                if (Objects.nonNull(node) && node.getPathVariableNode().getEndOfPath()) {
                    return node.getPathVariableNode();
                }
            } else {
                if (Objects.nonNull(node) && checkChildrenNotNull(node)) {
                    return getVal(node.getChildren(), key);
                }
            }
            return null;
        } else {
            if (isPathVariable(key)) {
                if (Objects.isNull(node.getPathVariableNode())) {
                    return null;
                }
                return this.getNode0(node.getPathVariableNode(), slice);
            } else {
                if (Objects.isNull(node) || Objects.isNull(node.getChildren()) || Objects.isNull(getVal(node.getChildren(), key))) {
                    return null;
                } else {
                    return this.getNode0(getVal(node.getChildren(), key), slice);
                }
            }
        }
    }

    /**
     * get current node biz info.
     *
     * @param trieNode trie
     * @return biz info
     */
    private Object getBizInfo(final ShenyuTrieNode trieNode) {
        return trieNode.getBizInfo();
    }

    /**
     * match all, when the path is /ab/c/**, that means /a/b/c/d can be matched.
     *
     * @param key key
     * @return match result
     */
    private static boolean isMatchAll(final String key) {
        return MATCH_ALL.equals(key);
    }

    /**
     * match wildcard, when the path is /a/b/*, the matched path maybe /a/b/c or /a/b/d and so on.
     *
     * @param key key
     * @return match result
     */
    private static boolean isMatchWildcard(final String key) {
        return WILDCARD.equals(key);
    }
    
    /**
     * determines whether the string is * or **.
     *
     * @param key the path key
     * @return true or false
     */
    private static boolean isMatchAllOrWildcard(final String key) {
        return isMatchAll(key) || isMatchWildcard(key);
    }
    
    /**
     * determines whether the string is path variable.
     *
     * @param key path string
     * @return true or false
     */
    private static boolean isPathVariable(final String key) {
        return Objects.nonNull(key) && key.startsWith("{") && key.endsWith("}");
    }

    private static <V> boolean containsKey(final Cache<String, V> cache, final String key) {
        V ret = getVal(cache, key);
        return Objects.nonNull(ret);
    }

    private static <V> V getVal(final Cache<String, V> cache, final String key) {
        if (Objects.nonNull(cache)) {
            return cache.getIfPresent(key);
        }
        return null;
    }
    
    private static <V> void cleanup(final Cache<String, V> cache) {
        if (Objects.nonNull(cache)) {
            cache.invalidateAll();
            cache.cleanUp();
        }
    }

    private static boolean checkChildrenNotNull(final ShenyuTrieNode shenyuTrieNode) {
        return Objects.nonNull(shenyuTrieNode) && Objects.nonNull(shenyuTrieNode.getChildren());
    }
    
    private static boolean checkPathRuleNotNull(final ShenyuTrieNode shenyuTrieNode) {
        return Objects.nonNull(shenyuTrieNode) && Objects.nonNull(shenyuTrieNode.getPathRuleCache());
    }
    
    private static boolean judgeEqual(final int param, final int actual) {
        return param == actual;
    }
}
