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

package org.apache.shenyu.plugin.ai.sensitive.word.ac;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;

/**
 * The Aho-Corasick multi pattern matching automaton.
 *
 * <p>An instance is built once for a dictionary and is immutable afterwards, so it can be shared
 * by concurrent requests. The failure links are built with a breadth first traversal of the trie,
 * which makes scanning a text linear in its length (times the length of the longest failure path).
 *
 * <p>Every word of the dictionary contained in the scanned text is reported, including nested and
 * overlapping ones, for example both {@code 中国} and {@code 中国银行} are reported for the text
 * {@code 中国银行}.
 */
public final class AhoCorasick {

    private static final AhoCorasick EMPTY_DICTIONARY = new AhoCorasick();

    private final TrieNode root = new TrieNode();

    private AhoCorasick() {
    }

    /**
     * Build an automaton for the given dictionary. Blank entries are ignored.
     *
     * @param words the dictionary words
     * @return the automaton
     */
    public static AhoCorasick of(final Collection<String> words) {
        if (Objects.isNull(words) || words.isEmpty()) {
            return EMPTY_DICTIONARY;
        }
        AhoCorasick automaton = new AhoCorasick();
        automaton.insertAll(words);
        automaton.buildFailureLinks();
        return automaton;
    }

    /**
     * An automaton without any word, it never matches. Useful as a fail open fallback when the
     * dictionary cannot be loaded.
     *
     * @return an empty automaton
     */
    public static AhoCorasick empty() {
        return EMPTY_DICTIONARY;
    }

    /**
     * Find every dictionary word contained in the given text.
     *
     * <p>The scan is linear in the length of the text: every node carries the words matched by
     * itself and by its failure chain, so no failure link is walked here.
     *
     * @param text the text to scan
     * @return the matched words, in the order they were found, each word is reported once
     */
    public Set<String> search(final String text) {
        if (Objects.isNull(text) || text.isEmpty()) {
            return Collections.emptySet();
        }
        Set<String> matches = new LinkedHashSet<>();
        TrieNode current = root;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            while (current != root && !current.children.containsKey(c)) {
                current = current.fail;
            }
            TrieNode next = current.children.get(c);
            current = Objects.isNull(next) ? root : next;
            if (!current.outputs.isEmpty()) {
                matches.addAll(current.outputs);
            }
        }
        return matches;
    }

    private void insertAll(final Collection<String> words) {
        for (String word : words) {
            if (Objects.isNull(word) || word.trim().isEmpty()) {
                continue;
            }
            insert(word.trim());
        }
    }

    private void insert(final String word) {
        TrieNode node = root;
        for (char c : word.toCharArray()) {
            node = node.children.computeIfAbsent(c, key -> new TrieNode());
        }
        node.word = word;
    }

    private void buildFailureLinks() {
        Queue<TrieNode> queue = new LinkedList<>();
        queue.add(root);
        while (!queue.isEmpty()) {
            TrieNode current = queue.poll();
            for (Map.Entry<Character, TrieNode> entry : current.children.entrySet()) {
                TrieNode child = entry.getValue();
                TrieNode fail = current.fail;
                while (Objects.nonNull(fail) && !fail.children.containsKey(entry.getKey())) {
                    fail = fail.fail;
                }
                child.fail = Objects.nonNull(fail) ? fail.children.get(entry.getKey()) : root;
                // Resolve the words matched at this node once, so that scanning a text never has
                // to walk the failure chain: the words of the failure chain are the suffixes.
                List<String> outputs = new ArrayList<>();
                if (Objects.nonNull(child.word)) {
                    outputs.add(child.word);
                }
                outputs.addAll(child.fail.outputs);
                child.outputs = outputs.isEmpty() ? Collections.emptyList() : outputs;
                queue.add(child);
            }
        }
    }

    /**
     * A trie node: {@code word} is not null only on the node that ends a dictionary word, and
     * {@code outputs} holds every word matched when the automaton is in this state.
     */
    private static final class TrieNode {

        private final Map<Character, TrieNode> children = new HashMap<>();

        private TrieNode fail;

        private String word;

        private List<String> outputs = Collections.emptyList();
    }
}
