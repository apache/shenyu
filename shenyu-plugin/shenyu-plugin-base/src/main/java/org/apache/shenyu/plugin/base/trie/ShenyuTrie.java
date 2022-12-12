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
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.TrieMatchModeEvent;

import java.util.Arrays;
import java.util.Objects;

public class ShenyuTrie {

    private static final String WILDCARD = "*";

    private static final String MATCH_ALL = "**";

    private ShenyuTrieNode root;

    private final Long childrenSize;

    private final Long pathRuleCacheSize;

    /**
     * the mode includes antPathMatch and pathPattern, please see {@linkplain TrieMatchModeEvent}.
     * antPathMatch means all full match, pathPattern is used in web.
     */
    private final String matchMode;

    public ShenyuTrie(final Long pathRuleCacheSize, final Long childrenSize, final String matchMode) {
        this.root = new ShenyuTrieNode("/", "/", false, pathRuleCacheSize);
        this.childrenSize = childrenSize;
        this.pathRuleCacheSize = pathRuleCacheSize;
        this.matchMode = matchMode;
    }

    /**
     * clear the trie.
     */
    public void clear() {
        this.root = null;
    }

    /**
     * judge the trie is empty.
     *
     * @param shenyuTrie trie
     * @return status
     */
    public boolean isEmpty(final ShenyuTrie shenyuTrie) {
        return shenyuTrie.root.getChildren().estimatedSize() == 0 && "/".equals(shenyuTrie.root.getMatchStr());
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
                    boolean endOfPath = isMatchAllOrWildcard(pathParts[i]) && i == pathParts.length - 1;
                    node = putNode0(pathParts[i], node, matchMode, endOfPath);
                }
                // after insert node, set full path and end of path
                node.setFullPath(uriPath);
                node.setEndOfPath(true);
                node.setBizInfo(bizInfo);
                if (Objects.isNull(node.getPathRuleCache())) {
                    node.setPathRuleCache(Caffeine.newBuilder().maximumSize(pathRuleCacheSize).build());
                }
                node.getPathRuleCache().put(ruleData.getSelectorId(), ruleData);
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
                return put(segment, shenyuTrieNode, true);
            }
            if (isMatchWildcard(segment)) {
                ShenyuTrieNode wildcardNode = put(segment, shenyuTrieNode, true);
                wildcardNode.setWildcard(true);
                return wildcardNode;
            }
        }
        if (TrieMatchModeEvent.ANT_PATH_MATCH.getMatchMode().equals(matchMode)) {
            if (isMatchAll(segment) && isPathEnd) {
                return put(segment, shenyuTrieNode, true);
            }
            if (isMatchWildcard(segment) && isPathEnd) {
                ShenyuTrieNode wildcardNode = put(segment, shenyuTrieNode, true);
                wildcardNode.setWildcard(true);
                return wildcardNode;
            }
        }
        // dynamic route
        if (segment.startsWith("{") && segment.endsWith("}")) {
            ShenyuTrieNode childNode;
            // contains key, get current pathVariable node
            if (Objects.nonNull(shenyuTrieNode.getPathVariablesSet()) && containsKey(shenyuTrieNode.getPathVariablesSet(), segment)) {
                childNode = getVal(shenyuTrieNode.getPathVariablesSet(), segment);
            } else {
                childNode = new ShenyuTrieNode();
                childNode.setMatchStr(segment);
                childNode.setEndOfPath(false);
                if (Objects.isNull(shenyuTrieNode.getPathVariablesSet())) {
                    shenyuTrieNode.setPathVariablesSet(Caffeine.newBuilder().maximumSize(childrenSize).build());
                }
                shenyuTrieNode.getPathVariablesSet().put(segment, childNode);
                shenyuTrieNode.setPathVariableNode(childNode);
            }
            return childNode;
        }
        return put(segment, shenyuTrieNode, false);
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
     * @param pluginName pluginName
     * @return {@linkplain ShenyuTrieNode}
     */
    public ShenyuTrieNode match(final String uriPath, final String selectorId, final String pluginName) {
        Objects.requireNonNull(selectorId);
        Objects.requireNonNull(pluginName);
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
                        if (endPath && checkPathRuleNotNull(currentNode)
                                && Objects.nonNull(getVal(currentNode.getPathRuleCache(), selectorId))
                                && pluginName.equals(getVal(currentNode.getPathRuleCache(), selectorId).getPluginName())) {
                            break;
                        }
                        // path is end and the match str is **, means match all
                        if (isMatchAll(currentNode.getMatchStr()) && currentNode.getEndOfPath()
                                && checkPathRuleNotNull(currentNode)
                                && Objects.nonNull(getVal(currentNode.getPathRuleCache(), selectorId))
                                && pluginName.equals(getVal(currentNode.getPathRuleCache(), selectorId).getPluginName())) {
                            break;
                        }
                    } else {
                        return null;
                    }
                }
                if (currentNode.getEndOfPath() || (Objects.nonNull(currentNode.getPathVariableNode()) && currentNode.getPathVariableNode().getEndOfPath())) {
                    return currentNode;
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
            if (checkChildrenNotNull(node) && containsKey(node.getChildren(), segment)) {
                return getVal(node.getChildren(), segment);
            }
            if (checkChildrenNotNull(node) && containsKey(node.getChildren(), WILDCARD)) {
                return getVal(node.getChildren(), WILDCARD);
            }
            if (checkChildrenNotNull(node) && containsKey(node.getChildren(), MATCH_ALL)) {
                return getVal(node.getChildren(), MATCH_ALL);
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
     * @param path path
     * @param selectorId selectorId
     */
    public void remove(final String path, final String selectorId) {
        if (StringUtils.isNotBlank(path)) {
            String strippedPath = StringUtils.strip(path, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            String key = pathParts[pathParts.length - 1];
            ShenyuTrieNode currentNode = this.getNode(path);
            // node is not null, judge exist many plugin mapping
            if (Objects.nonNull(currentNode) && Objects.nonNull(currentNode.getPathRuleCache())) {
                // check current mapping
                currentNode.getPathRuleCache().cleanUp();
                if (currentNode.getPathRuleCache().estimatedSize() == 1 && Objects.isNull(currentNode.getChildren())) {
                    // remove current node from parent node
                    String[] parentPathArray = Arrays.copyOfRange(pathParts, 0, pathParts.length - 1);
                    String parentPath = String.join("/", parentPathArray);
                    ShenyuTrieNode parentNode = this.getNode(parentPath);
                    parentNode.getChildren().invalidate(key);
                    parentNode.getChildren().cleanUp();
                } else {
                    // remove plugin mapping
                    currentNode.getPathRuleCache().invalidate(selectorId);
                    currentNode.getPathRuleCache().cleanUp();
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
                ShenyuTrieNode currentNode = root;
                return getNode0(currentNode, pathParts);
            } else {
                return null;
            }
        } else {
            return null;
        }
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
            if (isPathVariable(key)) {
                if (Objects.isNull(node.getPathVariableNode()) || !node.getPathVariableNode().getEndOfPath()) {
                    return null;
                }
                return node.getPathVariableNode();
            } else if (isMatchAllOrWildcard(key)) {
                return node;
            } else {
                if (Objects.isNull(node) || !checkChildrenNotNull(node)) {
                    return null;
                }
                return getVal(node.getChildren(), key);
            }
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

    private static boolean isMatchAllOrWildcard(final String key) {
        return isMatchAll(key) || isMatchWildcard(key);
    }
    
    private static boolean isPathVariable(final String key) {
        return Objects.nonNull(key) && key.startsWith("{") && key.endsWith("}");
    }

    private static <V> boolean containsKey(final Cache<String, V> cache, final String key) {
        V value = cache.getIfPresent(key);
        return Objects.nonNull(value);
    }

    private static <V> V getVal(final Cache<String, V> cache, final String key) {
        return cache.getIfPresent(key);
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
