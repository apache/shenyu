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

package org.apache.shenyu.plugin.base.event;

import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.enums.TrieEventEnum;
import org.springframework.context.ApplicationEvent;

/**
 * Rule trie event.
 */
public class TrieEvent extends ApplicationEvent {

    private static final long serialVersionUID = -6616858497711197175L;

    private final TrieEventEnum trieEventEnum;
    
    private final TrieCacheTypeEnum trieCacheTypeEnum;

    /**
     * shenyu trie event.
     *
     * @param trieEventEnum     ruleTrieEventEnum
     * @param trieCacheTypeEnum trie cache type
     * @param source            data
     */
    public <T> TrieEvent(final TrieEventEnum trieEventEnum, final TrieCacheTypeEnum trieCacheTypeEnum, final T source) {
        super(source);
        this.trieEventEnum = trieEventEnum;
        this.trieCacheTypeEnum = trieCacheTypeEnum;
    }

    /**
     * get rule trie build or remove event.
     *
     * @return {@linkplain TrieEventEnum} include insert and remove event
     */
    public TrieEventEnum getTrieEventEnum() {
        return trieEventEnum;
    }
    
    /**
     * get trie cache type.
     *
     * @return {@linkplain TrieCacheTypeEnum}
     */
    public TrieCacheTypeEnum getTrieCacheTypeEnum() {
        return trieCacheTypeEnum;
    }
}
