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

public class IdManagers {
    public enum Plugins {
        INSTANCE;
        
        private BiMap<String, String> name2id;
        public void set(BiMap<String, String> name2id) {
            this.name2id = name2id;
            System.out.println("plugin list: " + name2id); // TODO
        }
        
        public String getIdByName(String name) {
            return this.name2id.get(name);
        }
        
        public String getNameById(String id) {
            return this.name2id.inverse().get(id);
        }
    }
    
    public enum Rules {
        INSTANCE;
        
        private BiMap<String, String> name2id = HashBiMap.create();
    
        public void put(String name, String id) {
            name2id.put(name, id);
        }
        
        public String getIdByName(String name) {
            return this.name2id.get(name);
        }
        
        public String getNameById(String id) {
            return this.name2id.inverse().get(id);
        }
    }
    
    public enum Selectors {
        INSTANCE;
        
        private final BiMap<String, String> name2id = HashBiMap.create();
    
        public void put(String name, String id) {
            name2id.put(name, id);
        }
        
        public String getIdByName(String name) {
            return this.name2id.get(name);
        }
        
        public String getNameById(String id) {
            return this.name2id.inverse().get(id);
        }
    }
}
