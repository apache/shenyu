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
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.cache.WindowTinyLFUMap;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.enums.TrieMatchModeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ListUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class ShenyuTrie {
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuTrie.class);
    
    private static final String WILDCARD = "*";

    private static final String MATCH_ALL = "**";
    
    /**
     * when the trie is selector trie, the key is pluginName, when the trie is rule trie, the key is selectorId.
     */
    private final Map<String, ShenyuTrieNode> keyRootMap;
    
    /**
     * the mode includes antPathMatch and pathPattern
     * antPathMatch means all full match, pathPattern is used in web.
     */
    private final TrieMatchModeEnum matchMode;

    public ShenyuTrie(final Long cacheSize, final String matchMode) {
        this.matchMode = TrieMatchModeEnum.acquireTrieMatch(matchMode);
        this.keyRootMap = new WindowTinyLFUMap<>(cacheSize);
    }

    /**
     * clear the trie.
     */
    public void clear() {
        cleanup(this.keyRootMap);
    }

    /**
     * judge the trie is empty.
     *
     * @return status
     */
    public boolean isEmpty() {
        return this.keyRootMap.isEmpty();
    }

    /**
     * put node to trie.
     *
     * @param uriPaths uri path
     * @param source data
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
     * @param source rule data or selector data
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
        if (TrieMatchModeEnum.PATH_PATTERN.equals(matchMode)) {
            checkLegalPath(uriPath, pathParts);
        }
        RuleData ruleData = null;
        SelectorData selectorData = null;
        ShenyuTrieNode node;
        if (TrieCacheTypeEnum.RULE.equals(cacheType)) {
            ruleData = (RuleData) source;
            node = keyRootMap.computeIfAbsent(ruleData.getSelectorId(), key -> new ShenyuTrieNode("/", "/", false));
        } else {
            selectorData = (SelectorData) source;
            node = keyRootMap.computeIfAbsent(selectorData.getPluginName(), key -> new ShenyuTrieNode("/", "/", false));
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
            node.setPathRuleCache(new ConcurrentHashMap<>(Constants.TRIE_PATH_CACHE_SIZE));
        }
        if (TrieCacheTypeEnum.RULE.equals(cacheType)) {
            List<?> collections = node.getPathCache().get(ruleData.getSelectorId());
            if (CollectionUtils.isNotEmpty(collections)) {
                // synchronized list
                List<RuleData> ruleDataList = ListUtil.castList(collections, RuleData.class::cast);
                synchronized (ruleData.getId()) {
                    ruleDataList.add(ruleData);
                    ruleDataList.sort(Comparator.comparing(RuleData::getSort));
                    node.getPathCache().put(ruleData.getSelectorId(), ruleDataList);
                }
            } else {
                node.getPathCache().put(ruleData.getSelectorId(), Lists.newArrayList(ruleData));
            }
            node.setBizInfo(ruleData.getSelectorId());
            buildFailToNode(keyRootMap.get(ruleData.getSelectorId()));
        } else {
            List<?> collections = node.getPathCache().get(selectorData.getPluginName());
            if (CollectionUtils.isNotEmpty(collections)) {
                // synchronized list
                List<SelectorData> selectorDataList = ListUtil.castList(collections, SelectorData.class::cast);
                synchronized (selectorData.getId()) {
                    selectorDataList.add(selectorData);
                    selectorDataList.sort(Comparator.comparing(SelectorData::getSort));
                    node.getPathCache().put(selectorData.getPluginName(), selectorDataList);
                }
            } else {
                node.getPathCache().put(selectorData.getPluginName(), Lists.newArrayList(selectorData));
            }
            node.setBizInfo(selectorData.getPluginName());
            buildFailToNode(keyRootMap.get(selectorData.getPluginName()));
        }
    }
    
    private void buildFailToNode(final ShenyuTrieNode root) {
        if (Objects.isNull(root)) {
            return;
        }
        Queue<ShenyuTrieNode> queue = new LinkedList<>();
        Map<String, ShenyuTrieNode> pathVariableChildren = Optional.ofNullable(root.getPathVariables()).orElse(new HashMap<>(0));
        Map<String, ShenyuTrieNode> children = Optional.ofNullable(root.getChildren()).orElse(new HashMap<>(0));
        Map<String, ShenyuTrieNode> allChildren = new HashMap<>();
        allChildren.putAll(children);
        allChildren.putAll(pathVariableChildren);
        allChildren.forEach((key, currentNode) -> {
            currentNode.setFailToNode(root);
            queue.offer(currentNode);
        });
        while (!queue.isEmpty()) {
            ShenyuTrieNode currentNode = queue.poll();
            Map<String, ShenyuTrieNode> childrenList = Optional.ofNullable(currentNode.getChildren()).orElse(new HashMap<>(0));
            Map<String, ShenyuTrieNode> pathList = Optional.ofNullable(currentNode.getPathVariables()).orElse(new HashMap<>(0));
            Map<String, ShenyuTrieNode> newChildren = new HashMap<>();
            newChildren.putAll(childrenList);
            newChildren.putAll(pathList);
            if (allChildren.containsKey(currentNode.getMatchStr())) {
                newChildren.forEach((key, node) -> queue.offer(node));
                continue;
            }
            ShenyuTrieNode parent = currentNode.getParentNode();
            if (!isMatchWildcard(currentNode.getMatchStr()) && !isMatchAll(currentNode.getMatchStr()) && !isPathVariable(currentNode.getMatchStr())) {
                if (containsKey(parent.getChildren(), WILDCARD)) {
                    currentNode.setFailToNode(parent.getChildren().get(WILDCARD));
                } else if (containsKey(parent.getChildren(), MATCH_ALL)) {
                    currentNode.setFailToNode(parent.getChildren().get(MATCH_ALL));
                } else if (Objects.nonNull(parent.getPathVariableNode())) {
                    currentNode.setFailToNode(parent.getPathVariableNode());
                } else {
                    currentNode.setFailToNode(parent.getFailToNode());
                }
            } else if (isMatchWildcard(currentNode.getMatchStr())) {
                if (containsKey(parent.getChildren(), MATCH_ALL)) {
                    currentNode.setFailToNode(parent.getChildren().get(MATCH_ALL));
                } else if (Objects.nonNull(parent.getPathVariableNode())) {
                    currentNode.setFailToNode(parent.getPathVariableNode());
                } else {
                    currentNode.setFailToNode(parent.getFailToNode());
                }
            } else if (isMatchAll(currentNode.getMatchStr())) {
                if (currentNode.getEndOfPath()) {
                    currentNode.setFailToNode(null);
                } else if (Objects.nonNull(parent.getPathVariableNode())) {
                    currentNode.setFailToNode(parent.getPathVariableNode());
                } else {
                    currentNode.setFailToNode(parent.getFailToNode());
                }
            } else if (isPathVariable(currentNode.getMatchStr()) && MapUtils.isNotEmpty(parent.getPathVariables())) {
                if (parent.getPathVariables().size() == 1) {
                    currentNode.setFailToNode(parent.getFailToNode());
                } else {
                    if (Objects.isNull(currentNode.getFailToNode())) {
                        Queue<ShenyuTrieNode> pathVariableQueue = new LinkedList<>();
                        Map<String, ShenyuTrieNode> map = new HashMap<>(parent.getPathVariables());
                        map.remove(currentNode.getMatchStr());
                        map.values().forEach(pathVariableQueue::offer);
                        while (!pathVariableQueue.isEmpty()) {
                            ShenyuTrieNode pathVariableNode = pathVariableQueue.poll();
                            if (pathVariableQueue.size() > 1) {
                                currentNode.setFailToNode(pathVariableNode);
                                currentNode = pathVariableNode;
                            } else {
                                currentNode.setFailToNode(pathVariableNode);
                                pathVariableNode.setFailToNode(parent.getFailToNode());
                            }
                        }
                    }
                }
            } else {
                currentNode.setFailToNode(parent.getFailToNode());
            }
            newChildren.forEach((key, value) -> queue.offer(value));
        }
    }

    /**
     * match trie, trie exist and match the path will return current node <br>
     * match strategy: plugin get the node from trie and mark conflict, match sort: path, wildcard, match-all, path variable.
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
        ShenyuTrieNode currentNode = keyRootMap.get(bizInfo);
        int startIndex = 0;
        int[] matchAll = new int[pathParts.length];
        int[] wildcard = new int[pathParts.length];
        int[] pathVariable = new int[pathParts.length];
        ShenyuTrieNode matchNode;
        while (startIndex < pathParts.length) {
            String pathPart = pathParts[startIndex];
            if (Objects.isNull(currentNode)) {
                return null;
            }
            if (containsKey(currentNode.getChildren(), pathPart)) {
                currentNode = currentNode.getChildren().get(pathPart);
            } else if (hasWildcardNode(currentNode.getChildren(), pathPart) && Objects.nonNull(matchNode = findMatchWildcard(currentNode.getChildren(), pathPart)) && wildcard[startIndex] == 0) {
                currentNode = matchNode;
                wildcard[startIndex] = 1;
            } else if (containsKey(currentNode.getChildren(), MATCH_ALL) && matchAll[startIndex] == 0) {
                currentNode = currentNode.getChildren().get(MATCH_ALL);
                matchAll[startIndex] = 1;
                int matchAllIndex = startIndex;
                while (true) {
                    if (matchAllIndex == pathParts.length - 1) {
                        break;
                    }
                    matchAllIndex++;
                    if (containsKey(currentNode.getChildren(), pathParts[matchAllIndex])) {
                        currentNode = currentNode.getChildren().get(pathParts[matchAllIndex]);
                        startIndex = matchAllIndex;
                        break;
                    } else if (hasWildcardNode(currentNode.getChildren(), pathParts[matchAllIndex])
                            && Objects.nonNull(matchNode = findMatchWildcard(currentNode.getChildren(), pathParts[matchAllIndex]))) {
                        currentNode = matchNode;
                        wildcard[matchAllIndex] = 1;
                        startIndex = matchAllIndex;
                        break;
                    }
                }
            } else if (Objects.nonNull(currentNode.getPathVariableNode()) && currentNode.getPathVariables().size() == 1 && pathVariable[startIndex] == 0) {
                currentNode = currentNode.getPathVariableNode();
                pathVariable[startIndex] = 1;
            } else {
                // fail to match, reset the node to failToNode
                ShenyuTrieNode preParentNode = currentNode.getParentNode();
                ShenyuTrieNode newCurrentNode = currentNode.getFailToNode();
                // search failToNode's parentNode
                ShenyuTrieNode parentNode = newCurrentNode.getParentNode();
                if (Objects.isNull(parentNode) || Objects.nonNull(parentNode.getFailToNode()) && Objects.nonNull(newCurrentNode.getFailToNode())
                        && completeResolveConflict(parentNode, wildcard, matchAll, pathVariable, startIndex)
                        && parentNode.getFailToNode().equals(newCurrentNode.getFailToNode()) && "/".equals(parentNode.getParentNode().getMatchStr())) {
                    return null;
                }
                startIndex--;
                if (preParentNode.equals(parentNode)) {
                    startIndex--;
                    currentNode = parentNode.getParentNode();
                } else {
                    while (!preParentNode.equals(parentNode)) {
                        preParentNode = preParentNode.getParentNode();
                        startIndex--;
                    }
                    currentNode = parentNode;
                }
                continue;
            }
            if (startIndex < pathParts.length - 1 && Objects.nonNull(currentNode) && !currentNode.getEndOfPath()) {
                startIndex++;
                continue;
            }
            if (startIndex == pathParts.length - 1 && checkNode(currentNode, bizInfo) || Objects.nonNull(currentNode) && isMatchAll(currentNode.getMatchStr()) && checkNode(currentNode, bizInfo)) {
                return currentNode;
            }
        }
        return null;
    }
    
    private boolean completeResolveConflict(final ShenyuTrieNode node, final int[] wildcard, final int[] matchAll,
                                            final int[] pathVariable, final int index) {
        if (hasWildcardNode(node.getChildren(), WILDCARD) && containsKey(node.getChildren(), MATCH_ALL) && Objects.nonNull(node.getPathVariableNode())) {
            return wildcard[index] == 1 && matchAll[index] == 1 && pathVariable[index] == 1;
        } else if (hasWildcardNode(node.getChildren(), WILDCARD) && containsKey(node.getChildren(), MATCH_ALL)) {
            return wildcard[index] == 1 && matchAll[index] == 1;
        } else if (hasWildcardNode(node.getChildren(), WILDCARD) && Objects.nonNull(node.getPathVariableNode())) {
            return wildcard[index] == 1 && pathVariable[index] == 1;
        } else if (containsKey(node.getChildren(), MATCH_ALL) && Objects.nonNull(node.getPathVariableNode())) {
            return matchAll[index] == 1 && pathVariable[index] == 1;
        }
        return false;
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
     * <p> query node of the current path, if the node exists and the node exist the value of pathCache,
     * delete a rule with the same ruleId from pathCache or delete a selector by pluginName.</p>
     * <p> if current source data list is empty, children and pathVariablesSet is null,remove concurrent node from parent node.</p>
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
        ShenyuTrieNode currentNode;
        if (TrieCacheTypeEnum.RULE.equals(cacheType)) {
            RuleData ruleData = (RuleData) source;
            currentNode = this.getNode(path, ruleData.getSelectorId());
            Optional.ofNullable(currentNode).ifPresent(node -> {
                List<?> dataList = Optional.ofNullable(node.getPathCache()).map(cache -> cache.get(ruleData.getSelectorId())).orElse(Collections.emptyList());
                if (CollectionUtils.isNotEmpty(dataList)) {
                    removeRuleData(currentNode, pathParts, ruleData, dataList);
                }
            });
        } else {
            SelectorData selectorData = (SelectorData) source;
            currentNode = this.getNode(path, selectorData.getPluginName());
            Optional.ofNullable(currentNode).ifPresent(node -> {
                List<?> dataList = Optional.ofNullable(node.getPathCache()).map(cache -> cache.get(selectorData.getPluginName())).orElse(Collections.emptyList());
                if (CollectionUtils.isNotEmpty(dataList)) {
                    removeSelectorData(currentNode, pathParts, selectorData, dataList);
                }
            });
        }
    }
    
    /**
     * remove trie cache by key.
     *
     * @param key key
     */
    public void removeByKey(final String key) {
        keyRootMap.remove(key);
    }
    
    /**
     * getNode.
     *
     * @param uriPath uriPath
     * @param bizInfo source
     * @return {@link ShenyuTrieNode}
     */
    public ShenyuTrieNode getNode(final String uriPath, final String bizInfo) {
        if (StringUtils.isBlank(uriPath)) {
            return null;
        }
        String strippedPath = StringUtils.strip(uriPath, "/");
        String[] pathParts = StringUtils.split(strippedPath, "/");
        // get node from path pathParts
        ShenyuTrieNode node = keyRootMap.get(bizInfo);
        if (Objects.isNull(node)) {
            return null;
        }
        for (int i = 0; i < pathParts.length; i++) {
            String key = pathParts[i];
            if (Objects.nonNull(node)) {
                node = isPathVariable(key) && Objects.nonNull(node.getPathVariables())
                        ? node.getPathVariables().get(key) : getVal(node.getChildren(), key);
            }
            if (i == pathParts.length - 1) {
                return node;
            }
        }
        return null;
    }
    
    /**
     * get key root map key set.
     *
     * @return key set
     */
    public Set<String> getKeyRootKeys() {
        return keyRootMap.keySet();
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
            if (containsKey(shenyuTrieNode.getPathVariables(), segment)) {
                childNode = getVal(shenyuTrieNode.getPathVariables(), segment);
            } else {
                childNode = new ShenyuTrieNode();
                childNode.setMatchStr(segment);
                childNode.setParentNode(shenyuTrieNode);
                if (Objects.isNull(shenyuTrieNode.getPathVariables())) {
                    shenyuTrieNode.setPathVariables(new ConcurrentHashMap<>(Constants.TRIE_PATH_VARIABLES_SIZE));
                }
                shenyuTrieNode.getPathVariables().put(segment, childNode);
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
            shenyuTrieNode.setChildren(new ConcurrentHashMap<>(Constants.TRIE_CHILDREN_SIZE));
        }
        ShenyuTrieNode childrenNode;
        if (containsKey(shenyuTrieNode.getChildren(), segment)) {
            childrenNode = getVal(shenyuTrieNode.getChildren(), segment);
        } else {
            childrenNode = new ShenyuTrieNode();
            childrenNode.setMatchStr(segment);
            childrenNode.setParentNode(shenyuTrieNode);
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
        List<SelectorData> selectorDataList = ListUtil.castList(collection, SelectorData.class::cast);
        synchronized (selectorData.getId()) {
            selectorDataList.removeIf(selector -> selector.getId().equals(selectorData.getId()));
        }
        currentNode.getPathCache().put(selectorData.getPluginName(), selectorDataList);
        if (CollectionUtils.isEmpty(selectorDataList) && Objects.isNull(currentNode.getChildren())
                && Objects.isNull(currentNode.getPathVariables())) {
            removeData(pathParts, selectorData.getPluginName());
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
        List<RuleData> ruleDataList = ListUtil.castList(collection, RuleData.class::cast);
        synchronized (ruleData.getId()) {
            ruleDataList.removeIf(rule -> ruleData.getId().equals(rule.getId()));
        }
        currentNode.getPathCache().put(ruleData.getSelectorId(), ruleDataList);
        if (CollectionUtils.isEmpty(ruleDataList) && Objects.isNull(currentNode.getChildren())
                && Objects.isNull(currentNode.getPathVariables())) {
            removeData(pathParts, ruleData.getSelectorId());
        }
    }
    
    private void removeData(final String[] pathParts, final String source) {
        String key = pathParts[pathParts.length - 1];
        String[] parentPathArray = Arrays.copyOfRange(pathParts, 0, pathParts.length - 1);
        if (ArrayUtils.isEmpty(parentPathArray)) {
            return;
        }
        String parentPath = String.join("/", parentPathArray);
        ShenyuTrieNode parentNode = this.getNode(parentPath, source);
        if (Objects.isNull(parentNode)) {
            return;
        }
        Optional.ofNullable(parentNode.getChildren()).ifPresent(cache -> cache.remove(key));
    }

    private boolean hasWildcardNode(final Map<String, ShenyuTrieNode> children, final String key) {
        if (Objects.isNull(children)) {
            return false;
        }
        return children.values().stream().anyMatch(child -> isMatchWildcardPattern(key, child.getMatchStr()));
    }

    private static ShenyuTrieNode findMatchWildcard(final Map<String, ShenyuTrieNode> children, final String pathPart) {
        if (Objects.isNull(children)) {
            return null;
        }
        return children.values().stream().filter(child -> child.getWildcard() && isMatchWildcardPattern(pathPart, child.getMatchStr())).findFirst().orElse(null);
    }

    private static boolean isMatchWildcardPattern(final String segment, final String pattern) {
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

    private static boolean allStars(final String str, final int left, final int right) {
        for (int i = left; i < right; ++i) {
            if (str.charAt(i) != '*') {
                return false;
            }
        }
        return true;
    }
    
    private boolean checkNode(final ShenyuTrieNode currentNode, final String bizInfo) {
        return Objects.nonNull(currentNode) && currentNode.getEndOfPath()
                && bizInfo.equals(currentNode.getBizInfo()) && Objects.nonNull(currentNode.getPathCache())
                && CollectionUtils.isNotEmpty(currentNode.getPathCache().get(bizInfo));
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
}
