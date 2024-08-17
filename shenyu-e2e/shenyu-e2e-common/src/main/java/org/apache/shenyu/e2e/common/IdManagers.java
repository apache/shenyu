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

package org.apache.shenyu.e2e.common;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.StringJoiner;

/**
 * Id mangers.
 */
public class IdManagers {

    private static final Logger log = LoggerFactory.getLogger(IdManagers.class);

    public enum Plugins {
        INSTANCE;
        
        private BiMap<String, String> name2id;
        
        /**
         * set the name 2 id mapping BiMap.
         * @param name2id name2id
         */
        public void set(final BiMap<String, String> name2id) {
            this.name2id = name2id;
            StringJoiner joiner = new StringJoiner(",");
            for (Map.Entry<String, String> entry : name2id.entrySet()) {
                joiner.add(entry.getKey());
            }
            log.info("plugin list: {}", joiner);

        }
        
        /**
         * get id by name.
         * @param name name
         * @return String
         */
        public String getIdByName(final String name) {
            return this.name2id.get(name);
        }
        
        /**
         * get name by id.
         * @param id id
         * @return String
         */
        public String getNameById(final String id) {
            return this.name2id.inverse().get(id);
        }
    }
    
    public enum Rules {
        INSTANCE;
        
        private final BiMap<String, String> name2id = HashBiMap.create();
        
        /**
         * put name and id into the biMap of the single instance.
         * @param name name
         * @param id id
         */
        public void put(final String name, final String id) {
            name2id.put(name, id);
        }
        
        /**
         * get id by name.
         * @param name name
         * @return String
         */
        public String getIdByName(final String name) {
            return this.name2id.get(name);
        }
        
        /**
         * get name by id.
         * @param id id
         * @return String
         */
        public String getNameById(final String id) {
            return this.name2id.inverse().get(id);
        }
    }
    
    public enum Selectors {
        INSTANCE;
        
        private final BiMap<String, String> name2id = HashBiMap.create();
        
        /**
         * put name and id into the biMap of the single instance.
         * @param name name
         * @param id id
         */
        public void put(final String name, final String id) {
            name2id.put(name, id);
        }
        
        /**
         * get id by name.
         * @param name name
         * @return String
         */
        public String getIdByName(final String name) {
            return this.name2id.get(name);
        }
        
        /**
         * get name by id.
         * @param id id
         * @return String
         */
        public String getNameById(final String id) {
            return this.name2id.inverse().get(id);
        }
    }
}
