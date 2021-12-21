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

package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * FallbackShenyuClientRegisterService .
 */
public abstract class FallbackShenyuClientRegisterService implements ShenyuClientRegisterService {
    
    private final Logger logger = LoggerFactory.getLogger(FallbackShenyuClientRegisterService.class);
    
    /**
     * Register uri string.
     *
     * @param selectorName the selector name
     * @param uriList      the uri list
     * @return the string
     */
    @Override
    public String registerURI(final String selectorName, final List<URIRegisterDTO> uriList) {
        String result = ShenyuResultMessage.SUCCESS;
        try {
            String key = key(selectorName);
            ScheduledThread.remove(key);
            result = this.doRegisterURI(selectorName, uriList);
            if (!ScheduledThread.exist(key)) {
                logger.info("register success: {},{}", selectorName, uriList);
            }
        } catch (Exception ex) {
            logger.warn("register exception", ex);
        }
        return result;
    }
    
    /**
     * Recover.
     *
     * @param selectorName the selector name
     * @param uriList      the uri list
     */
    void recover(final String selectorName, final List<URIRegisterDTO> uriList) {
        if (uriList != null && !uriList.isEmpty()) {
            String key = key(selectorName);
            ScheduledThread.put(key, new FallbackHolder(this, selectorName, uriList));
            logger.info("register recovering wait retry: {},{}", selectorName, uriList);
        }
    }
    
    private String key(final String selectorName) {
        return String.join(":", selectorName, PluginNameAdapter.rpcTypeAdapter(rpcType()));
    }
    
    /**
     * Register uri 0 string.
     *
     * @param selectorName the selector name
     * @param uriList      the uri list
     * @return the string
     */
    abstract String doRegisterURI(String selectorName, List<URIRegisterDTO> uriList);
    
    /**
     * The type Fall holder.
     */
    private static final class FallbackHolder {
        
        private final String selectorName;
        
        private final List<URIRegisterDTO> uriList;
        
        private final FallbackShenyuClientRegisterService registerService;
        
        /**
         * Instantiates a new Fall holder.
         *
         * @param registerService the register service
         * @param selectorName    the selector name
         * @param uriList         the uri list
         */
        FallbackHolder(final FallbackShenyuClientRegisterService registerService, final String selectorName, final List<URIRegisterDTO> uriList) {
            this.registerService = registerService;
            this.selectorName = selectorName;
            this.uriList = uriList;
        }
        
        /**
         * Gets selector name.
         *
         * @return the selector name
         */
        public String getSelectorName() {
            return selectorName;
        }
        
        /**
         * Gets uri list.
         *
         * @return the uri list
         */
        public List<URIRegisterDTO> getUriList() {
            return uriList;
        }
        
        /**
         * Gets register service.
         *
         * @return the register service
         */
        public FallbackShenyuClientRegisterService getRegisterService() {
            return registerService;
        }
    }
    
    /**
     * The type Scheduled thread.
     */
    static final class ScheduledThread {
        
        private static final ScheduledThread INST = new ScheduledThread();
        
        private final Logger logger = LoggerFactory.getLogger(ScheduledThread.class);
        
        private final ScheduledExecutorService executorService = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("shenyu-client-register-fallback", false));
        
        private final Map<String, FallbackHolder> fallsRegisters = new ConcurrentHashMap<>();
        
        private ScheduledThread() {
            executorService.scheduleAtFixedRate(() -> {
                try {
                    retry();
                } catch (Exception ex) {
                    logger.warn("retry register failure...");
                }
            }, 0, 5, TimeUnit.SECONDS);
        }
        
        private void retry() {
            if (!fallsRegisters.isEmpty()) {
                Map<String, FallbackHolder> failed = new HashMap<>(fallsRegisters);
                if (failed.size() > 0) {
                    fallsRegisters.forEach((k, v) -> {
                        logger.info("retry register {}", v);
                        v.getRegisterService().registerURI(v.getSelectorName(), v.getUriList());
                    });
                }
            }
        }
        
        /**
         * Put.
         *
         * @param key        the key
         * @param fallHolder the fall holder
         */
        public static void put(final String key, final FallbackHolder fallHolder) {
            INST.doPut(key, fallHolder);
        }
        
        /**
         * Put.
         *
         * @param key        the key
         * @param fallHolder the fall holder
         */
        public void doPut(final String key, final FallbackHolder fallHolder) {
            this.fallsRegisters.put(key, fallHolder);
        }
        
        /**
         * Remove.
         *
         * @param key the key
         */
        public static void remove(final String key) {
            INST.doRemove(key);
        }
        
        /**
         * Remove 0.
         *
         * @param key the key
         */
        public void doRemove(final String key) {
            this.fallsRegisters.remove(key);
        }
        
        /**
         * Exist boolean.
         *
         * @param key the key
         * @return the boolean
         */
        public static boolean exist(final String key) {
            return INST.doExist(key);
        }
        
        /**
         * Exist 0 boolean.
         *
         * @param key the key
         * @return the boolean
         */
        public boolean doExist(final String key) {
            return this.fallsRegisters.containsKey(key);
        }
    }
}
