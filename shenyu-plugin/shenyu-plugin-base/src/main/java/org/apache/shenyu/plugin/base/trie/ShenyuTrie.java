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

import org.apache.commons.lang3.StringUtils;
import org.springframework.util.StopWatch;

import java.util.Arrays;
import java.util.Objects;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

public class ShenyuTrie {

    private static final ShenyuTrie INSTANCE = new ShenyuTrie();

    private ReentrantLock lock = new ReentrantLock(false);

    private static final String WILDCARD = "*";

    private static final String MATCH_ALL = "**";

    private ShenyuTrieNode root;

    private ConcurrentHashMap<String, String> cache = new ConcurrentHashMap<>();

    public ShenyuTrie() {
        this.root = new ShenyuTrieNode("/", "/", false);
    }

    public static ShenyuTrie getInstance() {
        return INSTANCE;
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
    public boolean isEmpty(ShenyuTrie shenyuTrie) {
        if (shenyuTrie.root.getChildren().isEmpty() && "/".equals(shenyuTrie.root.getMatchStr())) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * put node to trie.
     *
     * @param uriPath uri path
     */
    public void putNode(final String uriPath) {
        if (StringUtils.isNotBlank(uriPath)) {
            String strippedPath = StringUtils.strip(uriPath, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            if (pathParts.length > 0) {
                ShenyuTrieNode node = root;
                for (String segment : pathParts) {
                    node = putNode0(segment, node);
                    if (MATCH_ALL.equals(segment)) {
                        break;
                    }
                }
                // after insert node, set full path and end of path
                node.setFullPath(uriPath);
                node.setEndOfPath(true);
            }
        }
    }

    /**
     * put node to trie
     *
     * @param segment current string
     * @param shenyuTrieNode current trie node
     * @return {@linkplain ShenyuTrieNode}
     */
    private ShenyuTrieNode putNode0(final String segment, final ShenyuTrieNode shenyuTrieNode) {
        if (MATCH_ALL.equals(segment)) {
            shenyuTrieNode.setWildcard(true);
            return shenyuTrieNode;
        }
        if (Objects.isNull(shenyuTrieNode.getChildren())) {
            shenyuTrieNode.setChildren(new TreeMap<>());
        }
        ShenyuTrieNode childrenNode;
        if (shenyuTrieNode.getChildren().containsKey(segment)) {
            childrenNode = shenyuTrieNode.getChildren().get(segment);
        } else {
            childrenNode = new ShenyuTrieNode();
            childrenNode.setMatchStr(segment);
            childrenNode.setEndOfPath(false);
            shenyuTrieNode.getChildren().put(segment, childrenNode);
        }
        return childrenNode;
    }

    /**
     * match trie, trie exist and match the path will return true.
     *
     * @param uriPath uri path
     * @return match result
     */
    public boolean match(final String uriPath) {
        if (!StringUtils.isEmpty(uriPath)) {
            String strippedPath = StringUtils.strip(uriPath, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            if (pathParts.length > 0) {
                ShenyuTrieNode currentNode = root;
                for (String path : pathParts) {
                    currentNode = matchNode(path, currentNode);
                    // if not match route, exist the loop
                    if (Objects.isNull(currentNode) || currentNode.getWildcard()) {
                        break;
                    }
                }
                if (Objects.nonNull(currentNode) && currentNode.getEndOfPath()) {
                    return true;
                }
            }
        }
        return false;
    }

    private ShenyuTrieNode matchNode(final String segment, final ShenyuTrieNode node) {
        if (Objects.nonNull(node) && Objects.nonNull(node.getChildren()) && node.getChildren().containsKey(segment)) {
            return node.getChildren().get(segment);
        }
        // if the node is a wildcard, then return current node
        if (node.getWildcard()) {
            return node;
        }
        return null;
    }

    public void remove(final String path) {
        if (StringUtils.isNotBlank(path)) {
            String strippedPath = StringUtils.strip(path, "/");
            String[] pathParts = StringUtils.split(strippedPath, "/");
            String key = pathParts[pathParts.length-1];
            String[] parentPathArray = Arrays.copyOfRange(pathParts, 0, pathParts.length-1);
            String parentPath = String.join("/", parentPathArray);
            ShenyuTrieNode shenyuTrieNode = this.getNode(parentPath);
            // node is not null, remove this current
            if (Objects.nonNull(shenyuTrieNode)) {
                shenyuTrieNode.getChildren().remove(key);
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
     * @param node node
     * @param pathParts path parts
     * @return {@linkplain ShenyuTrieNode}
     */
    private ShenyuTrieNode getNode0(final ShenyuTrieNode node, final String[] pathParts) {
        // get key in path part arrays
        String key = pathParts[0];
        String[] slice = Arrays.copyOfRange(pathParts, 1, pathParts.length);
        // if exist one path
        if (slice.length == 0) {
            if (Objects.nonNull(node) && isMatchAll(key)) {
                return node;
            } else {
                if (Objects.isNull(node)) {
                    return null;
                } else {
                    return node.getChildren().get(key);
                }
            }
        } else {
            if (isMatchAll(key)) {
                return null;
            } else {
                if (Objects.isNull(node) || Objects.isNull(node.getChildren()) || Objects.isNull(node.getChildren().get(key))) {
                    return null;
                } else {
                    return this.getNode0(node.getChildren().get(key), slice);
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

    private boolean isPathVariableOrWildcard(final String key) {
        if (StringUtils.isBlank(key)) {
            return false;
        }
        if (WILDCARD.equals(key)) {
            return true;
        }
        return false;
    }

    private boolean isMatchAll(final String key) {
        return MATCH_ALL.equals(key);
    }

    public static void main(String[] args) {
        // System.gc();
// startMemory=totalMemory(总内存)-freeMemory(剩余内存)
//         long startMemory = Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory();
        StopWatch watch = new StopWatch("trie");
        watch.start();
        ShenyuTrie trie = new ShenyuTrie();
        // for(int i = 0; i < 1000000; i++) {
        //     trie.putNode("/" + i+1 + "/" + i+2 + "/" + i+3);
        // }
        trie.putNode("/02/03/04");
        trie.putNode("/**");
        trie.putNode("/a/b/**");
        watch.stop();
        System.out.println(watch.prettyPrint());

        StopWatch watch2 = new StopWatch("2");
        watch2.start();
        System.out.println(trie.match("/02/03/04"));;
        System.out.println(trie.match("/**"));
        System.out.println(trie.getNode("/02/03/04"));
        System.out.println(trie.getNode("/**"));
        System.out.println(trie.match("/0999/1000/1001"));;
        trie.remove("/02/03/04");
        System.out.println(trie.match("/02/03/04"));;
        System.out.println();
        watch2.stop();
        System.out.println(watch2.prettyPrint());
    }

}
