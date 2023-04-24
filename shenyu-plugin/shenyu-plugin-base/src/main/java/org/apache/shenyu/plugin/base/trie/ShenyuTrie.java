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
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.enums.TrieMatchModeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ListUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.BitSet;
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
        cleanup(this.root.getPathCache());
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
     * @param source rule data
     * @param cacheType cache type
     * @param <T> the data type
     */
    public <T> void putNode(final List<String> uriPaths, final T source, final TrieCacheTypeEnum cacheType) {
        if (CollectionUtils.isNotEmpty(uriPaths)) {
            uriPaths.forEach(path -> putNode(path, source, cacheType));
        }
    }

    /**
     * put node to trie, shenyu trie support *, **, path, pathVariable parameters.<br>
     * <p>* means match 0 or more character</p>
     * <p>** means match 0 or more dictory directory</p>
     * <p>pathVariable maybe like {name}</p>
     *
     * @param uriPath uri path
     * @param source rule data
     * @param cacheType cache type
     * @param <T> biz info type
     * @see org.springframework.util.AntPathMatcher
     * @see org.springframework.web.util.pattern.PathPattern
     */
    public <T> void putNode(final String uriPath, final T source, final TrieCacheTypeEnum cacheType) {
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
                remove(StringUtils.join(pathParts, "/", 0, i), source, cacheType);
                return;
            }
        }
        // after insert node, set full path and end of path
        node.setFullPath(uriPath);
        node.setEndOfPath(true);
        if (Objects.isNull(node.getPathCache())) {
            node.setPathRuleCache(new WindowTinyLFUMap<>(pathRuleCacheSize));
        }
        if (TrieCacheTypeEnum.RULE.equals(cacheType)) {
            RuleData ruleData = (RuleData) source;
            List<?> collections = node.getPathCache().get(ruleData.getSelectorId());
            if (CollectionUtils.isNotEmpty(collections)) {
                // synchronized list
                List<RuleData> ruleDataList = ListUtil.castLit(collections, RuleData.class);
                synchronized (ruleData.getId()) {
                    ruleDataList.add(ruleData);
                    ruleDataList.sort(Comparator.comparing(RuleData::getSort));
                    node.getPathCache().put(ruleData.getSelectorId(), ruleDataList);
                }
            } else {
                node.getPathCache().put(ruleData.getSelectorId(), Lists.newArrayList(ruleData));
            }
            node.setBizInfo(ruleData.getSelectorId());
        } else if (TrieCacheTypeEnum.SELECTOR.equals(cacheType)) {
            SelectorData selectorData = (SelectorData) source;
            List<?> collections = node.getPathCache().get(selectorData.getPluginName());
            if (CollectionUtils.isNotEmpty(collections)) {
                // synchronized list
                List<SelectorData> selectorDataList = ListUtil.castLit(collections, SelectorData.class);
                synchronized (selectorData.getId()) {
                    selectorDataList.add(selectorData);
                    selectorDataList.sort(Comparator.comparing(SelectorData::getSort));
                    node.getPathCache().put(selectorData.getPluginName(), selectorDataList);
                }
            } else {
                node.getPathCache().put(selectorData.getPluginName(), Lists.newArrayList(selectorData));
            }
            node.setBizInfo(selectorData.getPluginName());
        }
    }

    /**
     * match trie, trie exist and match the path will return current node <br>
     * match strategy: plugin get the node from trie and mark conflict, match sort: path > wildcard > match-all, path variable.
     * generally, wildcard, match-all, path-variable have the same matching rights as path, if there are the above path
     * matching variables in the child nodes of the current node, multi-marking will be performed.<br>
     * how to mark conflict: if current node has many conflict node, match mark number increment.<br>
     * match conflict: if current node has conflict node, mark the conflict with array index, traverse over trie,
     * if plugin can't get node from trie, check conflict, and resolve conflict, if plugin has resolve conflict
     * but can't get node from trie, return null.
     *
     * @param uriPath uri path
     * @param bizInfo bizInfo
     * @return {@linkplain ShenyuTrieNode}
     */
    public ShenyuTrieNode match(final String uriPath, final String bizInfo) {
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
        ShenyuTrieNode matchNode;
        while (startIndex < pathParts.length) {
            String pathPart = pathParts[startIndex];
            boolean endPath = startIndex == pathParts.length - 1;
            if (Objects.isNull(currentNode)) {
                return null;
            }
            if (containsKey(currentNode.getChildren(), pathPart)) {
                ShenyuTrieNode newNode = getVal(currentNode.getChildren(), pathPart);
                // current node maybe conflict with wildcard or matchAll
                if (currentNode.getChildren().containsKey(WILDCARD) && wildcard[startIndex] == 0) {
                    wildcard[startIndex] = 1;
                }
                if (currentNode.getChildren().containsKey(MATCH_ALL) && matchAll[startIndex] == 0) {
                    matchAll[startIndex] = 1;
                }
                currentNode = newNode;
                startIndex++;
                if (!endPath && Objects.nonNull(currentNode) && !currentNode.getEndOfPath()) {
                    continue;
                }
            } else if (hasWildcardNode(currentNode.getChildren(), pathPart) && wildcard[startIndex] == 0
                    && Objects.nonNull(matchNode = getAccessibleNode(wildcard, currentNode, startIndex, WILDCARD, pathPart))) {
                if (currentNode.getChildren().containsKey(MATCH_ALL) && matchAll[startIndex] == 0) {
                    matchAll[startIndex] = 1;
                }
                if (Objects.nonNull(currentNode.getPathVariableNode()) && pathVariable[startIndex] == 0) {
                    pathVariable[startIndex] = 1;
                }
                currentNode = matchNode;
                startIndex++;
                if (!endPath && !currentNode.getEndOfPath()) {
                    continue;
                }
            } else if (containsKey(currentNode.getChildren(), MATCH_ALL) && matchAll[startIndex] == 0
                    && Objects.nonNull(matchNode = getAccessibleNode(matchAll, currentNode, startIndex, MATCH_ALL, pathPart))) {
                if (Objects.nonNull(currentNode.getPathVariableNode()) && pathVariable[startIndex] == 0) {
                    pathVariable[startIndex] = 1;
                }
                currentNode = matchNode;
                startIndex++;
                if (!endPath && !currentNode.getEndOfPath()) {
                    continue;
                }
            } else if (Objects.nonNull(currentNode.getPathVariableNode()) && pathVariable[startIndex] == 0) {
                if (pathVariable[startIndex] == 0 && Objects.nonNull(currentNode.getPathVariableNode())) {
                    pathVariable[startIndex] = 1;
                }
                currentNode = currentNode.getPathVariableNode();
                startIndex++;
                if (!endPath && !currentNode.getEndOfPath()) {
                    continue;
                }
            }
            if (checkNode(endPath, currentNode, bizInfo)) {
                return currentNode;
            }
            // resolve conflict int mathAll, wildcard, pathVariable
            if (startIndex >= pathParts.length - 1) {
                Pair<Integer, ShenyuTrieNode> pair = resolveConflict(pathParts, wildcard, matchAll, pathVariable);
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
     * @param source source data
     * @param cacheType cache type
     * @param <T> selector data or rule data
     */
    public <T> void remove(final List<String> paths, final T source, final TrieCacheTypeEnum cacheType) {
        if (CollectionUtils.isNotEmpty(paths)) {
            paths.forEach(path -> remove(path, source, cacheType));
        }
    }

    /**
     * remove trie node.
     * <p> query node of the current path, if the node exists and the node exist the value of pathRuleCache,
     * delete a rule with the same ruleId from pathRuleCache.</p>
     * <p> if current rule data list is empty, children and pathVariablesSet is null,remove concurrent node from parent node.</p>
     *
     * @param path path
     * @param source source data
     * @param cacheType cache type
     * @param <T> selector data or rule data
     */
    public <T> void remove(final String path, final T source, final TrieCacheTypeEnum cacheType) {
        if (StringUtils.isBlank(path)) {
            return;
        }
        String strippedPath = StringUtils.strip(path, "/");
        String[] pathParts = StringUtils.split(strippedPath, "/");
        ShenyuTrieNode currentNode = this.getNode(path);
        // node is not null, judge exist many plugin mapping
        if (Objects.nonNull(currentNode) && Objects.nonNull(currentNode.getPathCache())) {
            if (TrieCacheTypeEnum.RULE.equals(cacheType)) {
                RuleData ruleData = (RuleData) source;
                List<?> dataList = currentNode.getPathCache().get(ruleData.getSelectorId());
                Optional.ofNullable(dataList).ifPresent(col -> removeRuleData(currentNode, pathParts, ruleData, col));
            } else if (TrieCacheTypeEnum.SELECTOR.equals(cacheType)) {
                SelectorData selectorData = (SelectorData) source;
                List<?> dataList = currentNode.getPathCache().get(selectorData.getPluginName());
                Optional.ofNullable(dataList).ifPresent(col -> removeSelectorData(currentNode, pathParts, selectorData, col));
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
                node = isPathVariable(key) && Objects.nonNull(node.getPathVariablesSet())
                        ? node.getPathVariablesSet().get(key) : getVal(node.getChildren(), key);
            }
            if (i == pathParts.length - 1) {
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
    
    /**
     * remove selector data.
     *
     * @param currentNode current node
     * @param pathParts path parts
     * @param selectorData selector data
     * @param collection selector data list
     */
    private void removeSelectorData(final ShenyuTrieNode currentNode, final String[] pathParts,
                                    final SelectorData selectorData, final List<?> collection) {
        List<SelectorData> selectorDataList = ListUtil.castLit(collection, SelectorData.class);
        synchronized (selectorData.getId()) {
            selectorDataList.removeIf(selector -> selector.getId().equals(selectorData.getId()));
        }
        currentNode.getPathCache().put(selectorData.getPluginName(), selectorDataList);
        if (CollectionUtils.isEmpty(selectorDataList) && Objects.isNull(currentNode.getChildren())
                && Objects.isNull(currentNode.getPathVariablesSet())) {
            removeData(pathParts);
        }
    }
    
    /**
     * remove rule data.
     *
     * @param currentNode current node
     * @param pathParts path parts
     * @param ruleData rule data
     * @param collection rule data list
     */
    private void removeRuleData(final ShenyuTrieNode currentNode, final String[] pathParts,
                                final RuleData ruleData, final List<?> collection) {
        // check current mapping
        List<RuleData> ruleDataList = ListUtil.castLit(collection, RuleData.class);
        synchronized (ruleData.getId()) {
            ruleDataList.removeIf(rule -> ruleData.getId().equals(rule.getId()));
        }
        currentNode.getPathCache().put(ruleData.getSelectorId(), ruleDataList);
        if (CollectionUtils.isEmpty(ruleDataList) && Objects.isNull(currentNode.getChildren())
                && Objects.isNull(currentNode.getPathVariablesSet())) {
            removeData(pathParts);
        }
    }
    
    private void removeData(final String[] pathParts) {
        String key = pathParts[pathParts.length - 1];
        String[] parentPathArray = Arrays.copyOfRange(pathParts, 0, pathParts.length - 1);
        if (ArrayUtils.isEmpty(parentPathArray)) {
            return;
        }
        String parentPath = String.join("/", parentPathArray);
        ShenyuTrieNode parentNode = this.getNode(parentPath);
        parentNode.getChildren().remove(key);
    }

    private boolean hasWildcardNode(final Map<String, ShenyuTrieNode> children, final String key) {
        if (Objects.isNull(children)) {
            return false;
        }
        return children.values().stream().anyMatch(ShenyuTrieNode::getWildcard);
    }
    
    private ShenyuTrieNode getAccessibleNode(final int[] arr, final ShenyuTrieNode node, final int startIndex, final String type, final String pathPart) {
        if (WILDCARD.equals(type) && arr[startIndex] == 0) {
            ShenyuTrieNode wildcardNode = findMatchWildcard(node.getChildren(), pathPart);
            if (Objects.nonNull(wildcardNode)) {
                arr[startIndex] = 1;
                return wildcardNode;
            }
        }
        if (MATCH_ALL.equals(type) && arr[startIndex] == 0) {
            ShenyuTrieNode matchAllNode = getVal(node.getChildren(), MATCH_ALL);
            if (Objects.nonNull(matchAllNode)) {
                arr[startIndex] = 1;
                return matchAllNode;
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
    
    private boolean checkNode(final boolean endPath, final ShenyuTrieNode currentNode, final String bizInfo) {
        return endPath && Objects.nonNull(currentNode) && currentNode.getEndOfPath()
                && bizInfo.equals(currentNode.getBizInfo()) && Objects.nonNull(currentNode.getPathCache())
                && CollectionUtils.isNotEmpty(currentNode.getPathCache().get(bizInfo));
    }
    
    private boolean hasConflict(final int[] wildcard, final int[] matchAll, final int[] pathVariable) {
        return ArrayUtils.contains(wildcard, 1) && ArrayUtils.contains(matchAll, 1)
                || ArrayUtils.contains(wildcard, 1) && ArrayUtils.contains(pathVariable, 1)
                || ArrayUtils.contains(matchAll, 1) && ArrayUtils.contains(pathVariable, 1);
    }
    
    private Pair<Integer, ShenyuTrieNode> resolveConflict(final String[] pathParts, final int[] wildcard, final int[] matchAll, final int[] pathVariable) {
        BitSet wildcardSet = ArrayUtils.indexesOf(wildcard, 1);
        BitSet matchAllSet = ArrayUtils.indexesOf(matchAll, 1);
        BitSet pathVariableSet = ArrayUtils.indexesOf(pathVariable, 1);
        if (!wildcardSet.isEmpty()) {
            int index = wildcardSet.nextSetBit(0);
            wildcard[index] = -1;
            return Pair.of(0, this.getNode(StringUtils.join(pathParts, "/", 0, index)));
        } else if (!matchAllSet.isEmpty()) {
            int index = matchAllSet.nextSetBit(0);
            matchAll[index] = -1;
            return Pair.of(0, this.getNode(StringUtils.join(pathParts, "/", 0, index)));
        } else if (!pathVariableSet.isEmpty()) {
            int index = pathVariableSet.nextSetBit(0);
            pathVariable[index] = -1;
            return Pair.of(0, this.getNode(StringUtils.join(pathParts, "/", 0, index)));
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
        return Objects.nonNull(cache) && cache.containsKey(key);
    }

    private static <V> V getVal(final Map<String, V> cache, final String key) {
        return Objects.nonNull(cache) ? cache.get(key) : null;
    }
    
    private static <V> void cleanup(final Map<String, V> cache) {
        if (Objects.nonNull(cache)) {
            cache.clear();
        }
    }

    private static boolean checkChildrenNotNull(final ShenyuTrieNode shenyuTrieNode) {
        return Objects.nonNull(shenyuTrieNode) && Objects.nonNull(shenyuTrieNode.getChildren());
    }
}
