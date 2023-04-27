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

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.TrieMatchModeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

public class ShenyuTrie {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuTrie.class);
    
    private static final String WILDCARD = "*";

    private static final String MATCH_ALL = "**";

    private final ShenyuTrieNode root;

    private final Long childrenSize;

    private final Long pathRuleCacheSize;
    
    private final Long pathVariableSize;

    /**
     * the mode includes antPathMatch and pathPattern
     * antPathMatch means all full match, pathPattern is used in web.
     */
    private final TrieMatchModeEnum matchMode;

    public ShenyuTrie(final Long childrenSize, final Long pathRuleCacheSize, final Long pathVariableSize, final String matchMode) {
        this.root = new ShenyuTrieNode("/", "/", false, childrenSize, pathRuleCacheSize, pathVariableSize);
        this.childrenSize = childrenSize;
        this.pathRuleCacheSize = pathRuleCacheSize;
        this.pathVariableSize = pathVariableSize;
        this.matchMode = TrieMatchModeEnum.acquireTrieMatch(matchMode);
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
        return this.root.getChildren().size() == 0
                && this.root.getPathVariablesSet().size() == 0
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
     * put node to trie, shenyu trie support *, **, path, pathVariable parameters.<br>
     * <p>* means match 0 or more character</p>
     * <p>** means match 0 or more dictory directory</p>
     * <p>pathVariable maybe like {name}</p>
     *
     * @param uriPath uri path
     * @param ruleData rule data
     * @param bizInfo biz info
     * @see org.springframework.util.AntPathMatcher
     * @see org.springframework.web.util.pattern.PathPattern
     */
    public void putNode(final String uriPath, final RuleData ruleData, final Object bizInfo) {
        if (StringUtils.isBlank(uriPath)) {
            return;
        }
        String strippedPath = StringUtils.strip(uriPath, "/");
        String[] pathParts = StringUtils.split(strippedPath, "/");
        if (ArrayUtils.isEmpty(pathParts)) {
            return;
        }
        ShenyuTrieNode node = root;
        if (TrieMatchModeEnum.PATH_PATTERN.equals(matchMode)) {
            checkLegalPath(uriPath, pathParts);
        }
        for (int i = 0; i < pathParts.length; i++) {
            node = putNode0(pathParts[i], node);
            if (Objects.isNull(node)) {
                remove(StringUtils.join(pathParts, "/", 0, i), ruleData);
                return;
            }
        }
        // after insert node, set full path and end of path
        node.setFullPath(uriPath);
        node.setEndOfPath(true);
        node.setSelectorId(ruleData.getSelectorId());
        node.setBizInfo(bizInfo);
        if (Objects.isNull(node.getPathRuleCache())) {
            node.setPathRuleCache(new WindowTinyLFUMap<>(pathRuleCacheSize));
        }
        List<RuleData> ruleDataList = getVal(node.getPathRuleCache(), ruleData.getSelectorId());
        if (CollectionUtils.isNotEmpty(ruleDataList)) {
            // synchronized list
            synchronized (ruleData.getId()) {
                ruleDataList.add(ruleData);
                ruleDataList.sort(Comparator.comparing(RuleData::getSort));
                node.getPathRuleCache().put(ruleData.getSelectorId(), ruleDataList);
            }
        } else {
            node.getPathRuleCache().put(ruleData.getSelectorId(), Lists.newArrayList(ruleData));
        }
    }

    /**
     * match trie, trie exist and match the path will return current node.
     *
     * @param uriPath uri path
     * @param selectorId selectorId
     * @return {@linkplain ShenyuTrieNode}
     */
    public ShenyuTrieNode match(final String uriPath, final String selectorId) {
        String strippedPath = StringUtils.strip(uriPath, "/");
        String[] pathParts = StringUtils.split(strippedPath, "/");
        if (ArrayUtils.isEmpty(pathParts)) {
            return null;
        }
        int startIndex = 0;
        ShenyuTrieNode currentNode = root;
        int[] matchAll = new int[pathParts.length];
        int[] wildcard = new int[pathParts.length];
        int[] pathVariable = new int[pathParts.length];
        ShenyuTrieNode matchNode = null;
        while (startIndex < pathParts.length) {
            String pathPart = pathParts[startIndex];
            boolean endPath = Integer.compare(startIndex, pathParts.length - 1) == 0;
            if (Objects.isNull(currentNode)) {
                return null;
            }
            if (containsKey(currentNode.getChildren(), pathPart)) {
                currentNode = getVal(currentNode.getChildren(), pathPart);
                startIndex++;
                if (!endPath && Objects.nonNull(currentNode) && !currentNode.getEndOfPath()) {
                    continue;
                }
            } else if (hasWildcardNode(currentNode.getChildren(), pathPart) && wildcard[startIndex] == 0
                    && Objects.nonNull(matchNode = getAccessAbleNode(wildcard, currentNode, startIndex, WILDCARD, pathPart))) {
                currentNode = matchNode;
                startIndex++;
                if (!endPath && !currentNode.getEndOfPath()) {
                    continue;
                }
            } else if (containsKey(currentNode.getChildren(), MATCH_ALL) && matchAll[startIndex] == 0
                    && Objects.nonNull(matchNode = getAccessAbleNode(matchAll, currentNode, startIndex, MATCH_ALL, pathPart))) {
                currentNode = matchNode;
                startIndex++;
                if (!endPath && !currentNode.getEndOfPath()) {
                    continue;
                }
            } else if (Objects.nonNull(currentNode.getPathVariableNode())) {
                if (pathVariable[startIndex] == 0 && Objects.nonNull(currentNode.getPathVariableNode())) {
                    pathVariable[startIndex] = 1;
                }
                currentNode = currentNode.getPathVariableNode();
                startIndex++;
                if (!endPath && !currentNode.getEndOfPath()) {
                    continue;
                }
            }
            if (checkNode(endPath, currentNode, selectorId)) {
                return currentNode;
            }
            // resolve conflict int mathAll, wildcard, pathVariable
            if (startIndex >= pathParts.length - 1) {
                Pair<Integer, ShenyuTrieNode> pair = resolveConflict(wildcard, matchAll, pathVariable);
                if (Objects.nonNull(pair)) {
                    startIndex = pair.getLeft();
                    currentNode = pair.getRight();
                } else {
                    return null;
                }
            } else {
                startIndex++;
            }
        }
        return null;
    }


    /**
     * remove trie node.
     *
     * @param paths path list
     * @param ruleData rule data
     */
    public void remove(final List<String> paths, final RuleData ruleData) {
        if (CollectionUtils.isNotEmpty(paths)) {
            paths.forEach(path -> remove(path, ruleData));
        }
    }

    /**
     * remove trie node.
     * <p> query node of the current path, if the node exists and the node exist the value of pathRuleCache,
     * delete a rule with the same ruleId from pathRuleCache.</p>
     * <p> if current rule data list is empty, children and pathVariablesSet is null,remove concurrent node from parent node.</p>
     *
     * @param path path
     * @param ruleData ruleData
     */
    public void remove(final String path, final RuleData ruleData) {
        Objects.requireNonNull(ruleData.getId(), "rule id cannot be empty");
        if (StringUtils.isBlank(path)) {
            return;
        }
        String strippedPath = StringUtils.strip(path, "/");
        String[] pathParts = StringUtils.split(strippedPath, "/");
        String key = pathParts[pathParts.length - 1];
        ShenyuTrieNode currentNode = this.getNode(path);
        // node is not null, judge exist many plugin mapping
        if (Objects.nonNull(currentNode) && Objects.nonNull(currentNode.getPathRuleCache())) {
            // check current mapping
            List<RuleData> ruleDataList = getVal(currentNode.getPathRuleCache(), ruleData.getSelectorId());
            ruleDataList = Optional.ofNullable(ruleDataList).orElse(Collections.emptyList());
            synchronized (ruleData.getId()) {
                ruleDataList.removeIf(rule -> ruleData.getId().equals(rule.getId()));
            }
            if (CollectionUtils.isEmpty(ruleDataList) && Objects.isNull(currentNode.getChildren())
                    && Objects.isNull(currentNode.getPathVariablesSet())) {
                // remove current node from parent node
                String[] parentPathArray = Arrays.copyOfRange(pathParts, 0, pathParts.length - 1);
                String parentPath = String.join("/", parentPathArray);
                ShenyuTrieNode parentNode = this.getNode(parentPath);
                parentNode.getChildren().remove(key);
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
        if (StringUtils.isBlank(uriPath)) {
            return null;
        }
        String strippedPath = StringUtils.strip(uriPath, "/");
        String[] pathParts = StringUtils.split(strippedPath, "/");
        // get node from path pathParts
        ShenyuTrieNode node = root;
        for (int i = 0; i < pathParts.length; i++) {
            String key = pathParts[i];
            if (Objects.nonNull(node)) {
                if (isPathVariable(key) && Objects.nonNull(node.getPathVariablesSet())) {
                    node = node.getPathVariablesSet().get(key);
                } else {
                    node = getVal(node.getChildren(), key);
                }
            }
            if (Integer.compare(i, pathParts.length - 1) == 0) {
                return node;
            }
        }
        return null;
    }

    /**
     * check legal path.
     *
     * @param pathParts path array
     */
    private void checkLegalPath(final String uriPath, final String[] pathParts) {
        //int position = Arrays.binarySearch(pathParts, 0, pathParts.length, MATCH_ALL);
        for (int i = 0; i < pathParts.length - 1 /* not the end of path */; i++) {
            if (isMatchAll(pathParts[i])) {
                LOG.error("error path:{}, error position:{}", uriPath, i);
                throw new ShenyuException("No more pattern data allowed after ** pattern element");
            }
        }
    }

    /**
     * put node to trie.
     *
     * @param segment current string
     * @param shenyuTrieNode current trie node
     * @return {@linkplain ShenyuTrieNode}
     */
    private ShenyuTrieNode putNode0(final String segment, final ShenyuTrieNode shenyuTrieNode) {
        if (isMatchWildcard(segment)) {
            ShenyuTrieNode wildcardNode = this.put(segment, shenyuTrieNode);
            wildcardNode.setWildcard(true);
        } else if (isPathVariable(segment)) {
            /* dynamic route */
            ShenyuTrieNode childNode;
            // contains key, get current pathVariable node
            if (containsKey(shenyuTrieNode.getPathVariablesSet(), segment)) {
                childNode = getVal(shenyuTrieNode.getPathVariablesSet(), segment);
            } else {
                childNode = new ShenyuTrieNode();
                childNode.setMatchStr(segment);
                if (Objects.isNull(shenyuTrieNode.getPathVariablesSet())) {
                    shenyuTrieNode.setPathVariablesSet(new WindowTinyLFUMap<>(pathVariableSize));
                }
                shenyuTrieNode.getPathVariablesSet().put(segment, childNode);
                shenyuTrieNode.setPathVariableNode(childNode);
            }
            return childNode;
        }
        return this.put(segment, shenyuTrieNode);
    }

    /**
     * put node.
     *
     * @param segment segment
     * @param shenyuTrieNode shenyu trie node
     * @return ShenyuTrieNode
     */
    private ShenyuTrieNode put(final String segment, final ShenyuTrieNode shenyuTrieNode) {
        if (Objects.isNull(shenyuTrieNode.getChildren())) {
            shenyuTrieNode.setChildren(new WindowTinyLFUMap<>(childrenSize));
        }
        ShenyuTrieNode childrenNode;
        if (containsKey(shenyuTrieNode.getChildren(), segment)) {
            childrenNode = getVal(shenyuTrieNode.getChildren(), segment);
        } else {
            childrenNode = new ShenyuTrieNode();
            childrenNode.setMatchStr(segment);
            shenyuTrieNode.getChildren().put(segment, childrenNode);
        }
        return childrenNode;
    }

    private boolean hasWildcardNode(final Map<String, ShenyuTrieNode> children, final String key) {
        if (Objects.isNull(children)) {
            return false;
        }
        return children.values().stream().anyMatch(ShenyuTrieNode::getWildcard);
    }
    
    private ShenyuTrieNode getAccessAbleNode(final int[] arr, final ShenyuTrieNode node, final int startIndex, final String type, final String pathPart) {
        if (WILDCARD.equals(type)) {
            if (arr[startIndex] == 0) {
                ShenyuTrieNode wildcardNode = findMatchWildcard(node.getChildren(), pathPart);
                if (Objects.nonNull(wildcardNode)) {
                    arr[startIndex] = 1;
                    return wildcardNode;
                }
            }
        }
        if (MATCH_ALL.equals(type)) {
            if (arr[startIndex] == 0) {
                ShenyuTrieNode matchAllNode = getVal(node.getChildren(), MATCH_ALL);
                if (Objects.nonNull(matchAllNode)) {
                    arr[startIndex] = 1;
                    return matchAllNode;
                }
            }
        }
        return null;
    }

    private ShenyuTrieNode findMatchWildcard(final Map<String, ShenyuTrieNode> children, final String pathPart) {
        if (Objects.isNull(children)) {
            return null;
        }
        return children.values().stream().filter(child -> child.getWildcard() && isMatchWildcardPattern(pathPart, child.getMatchStr())).findFirst().orElse(null);
    }

    private boolean isMatchWildcardPattern(final String segment, final String pattern) {
        int sRight = segment.length();
        int pRight = pattern.length();
        while (sRight > 0 && pRight > 0 && pattern.charAt(pRight - 1) != '*') {
            if (segment.charAt(sRight - 1) == pattern.charAt(pRight - 1)) {
                --sRight;
                --pRight;
            } else {
                return false;
            }
        }

        if (pRight == 0) {
            return sRight == 0;
        }

        int sIndex = 0;
        int pIndex = 0;
        int sRecord = -1;
        int pRecord = -1;

        while (sIndex < sRight && pIndex < pRight) {
            if (pattern.charAt(pIndex) == '*') {
                ++pIndex;
                sRecord = sIndex;
                pRecord = pIndex;
            } else if (segment.charAt(sIndex) == pattern.charAt(pIndex)) {
                ++sIndex;
                ++pIndex;
            } else if (sRecord != -1 && sRecord + 1 < sRight) {
                ++sRecord;
                sIndex = sRecord;
                pIndex = pRecord;
            } else {
                return false;
            }
        }

        return allStars(pattern, pIndex, pRight);
    }

    private boolean allStars(final String str, final int left, final int right) {
        for (int i = left; i < right; ++i) {
            if (str.charAt(i) != '*') {
                return false;
            }
        }
        return true;
    }
    
    private boolean checkNode(final boolean endPath, final ShenyuTrieNode currentNode, final String selectorId) {
        return endPath && Objects.nonNull(currentNode) && currentNode.getEndOfPath()
                && selectorId.equals(currentNode.getSelectorId()) && Objects.nonNull(currentNode.getPathRuleCache())
                && CollectionUtils.isNotEmpty(currentNode.getPathRuleCache().get(selectorId));
    }
    
    private Pair<Integer, ShenyuTrieNode> resolveConflict(final int[] wildcard, final int[] matchAll, final int[] pathVariable) {
        BitSet wildcardSet = ArrayUtils.indexesOf(wildcard, 1);
        BitSet matchAllSet = ArrayUtils.indexesOf(matchAll, 1);
        BitSet pathVariableSet = ArrayUtils.indexesOf(pathVariable, 1);
        if (!wildcardSet.isEmpty()) {
            wildcard[wildcardSet.nextSetBit(0)] = -1;
            return Pair.of(0, root);
        } else if (!matchAllSet.isEmpty()) {
            matchAll[matchAllSet.nextSetBit(0)] = -1;
            return Pair.of(0, root);
        } else if (!pathVariableSet.isEmpty()) {
            pathVariable[pathVariableSet.nextSetBit(0)] = -1;
            return Pair.of(0, root);
        } else {
            return null;
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
        return !isPathVariable(key) && !isMatchAll(key) && Objects.nonNull(key) && key.contains(WILDCARD);
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

    private static <V> boolean containsKey(final Map<String, V> cache, final String key) {
        if (Objects.isNull(cache)) {
            return false;
        }
        return cache.containsKey(key);
    }

    private static <V> V getVal(final Map<String, V> cache, final String key) {
        if (Objects.nonNull(cache)) {
            return cache.get(key);
        }
        return null;
    }
    
    private static <V> void cleanup(final Map<String, V> cache) {
        if (Objects.nonNull(cache)) {
            cache.clear();
        }
    }

    private static boolean checkChildrenNotNull(final ShenyuTrieNode shenyuTrieNode) {
        return Objects.nonNull(shenyuTrieNode) && Objects.nonNull(shenyuTrieNode.getChildren());
    }
    
    private static boolean checkPathRuleNotNull(final ShenyuTrieNode shenyuTrieNode) {
        return Objects.nonNull(shenyuTrieNode) && Objects.nonNull(shenyuTrieNode.getPathRuleCache());
    }

}
