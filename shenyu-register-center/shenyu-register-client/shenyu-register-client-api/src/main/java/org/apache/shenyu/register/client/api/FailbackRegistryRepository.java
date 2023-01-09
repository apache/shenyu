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

package org.apache.shenyu.register.client.api;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.register.client.api.retry.FailureRegistryTask;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * FailbackRegistryRepository .
 */
public abstract class FailbackRegistryRepository implements ShenyuClientRegisterRepository {
    
    private final Logger logger = LoggerFactory.getLogger(FailbackRegistryRepository.class);
    
    private final Map<String, Holder> concurrentHashMap = new ConcurrentHashMap<>();
    
    private final Timer timer;
    
    /**
     * Instantiates a new Failback registry repository.
     */
    public FailbackRegistryRepository() {
        this.timer = WheelTimerFactory.getSharedTimer();
    }
    
    /**
     * Persist metadata.
     *
     * @param metadata metadata
     */
    @Override
    public void persistInterface(final MetaDataRegisterDTO metadata) {
        try {
            this.doPersistInterface(metadata);
        } catch (Exception ex) {
            //If a failure occurs, it needs to be added to the retry list.
            logger.warn("Failed to persistInterface {}, cause:{}", metadata, ex.getMessage());
            this.addFailureMetaDataRegister(metadata);
        }
    }
    
    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void persistURI(final URIRegisterDTO registerDTO) {
        try {
            this.doPersistURI(registerDTO);
        } catch (Exception ex) {
            //If a failure occurs, it needs to be added to the retry list.
            logger.warn("Failed to persistURI {}, cause:{}", registerDTO, ex.getMessage());
            this.addFailureUriDataRegister(registerDTO);
        }
    }

    /**
     * Persist apiDoc.
     * @param registerDTO registerDTO
     */
    @Override
    public void persistApiDoc(final ApiDocRegisterDTO registerDTO) {
        try {
            this.doPersistApiDoc(registerDTO);
        } catch (Exception ex) {
            //TODO error retry
            //If a failure occurs, it needs to be added to the retry list.
        }
    }

    /**
     * doPersistApiDoc.
     * @param apiDocRegisterDTO apiDocRegisterDTO
     */
    protected abstract void doPersistApiDoc(ApiDocRegisterDTO apiDocRegisterDTO);
    
    /**
     * Add failure meta data register.
     *
     * @param <T> the type parameter
     * @param t   the t
     */
    protected <T> void addFailureMetaDataRegister(final T t) {
        if (t instanceof MetaDataRegisterDTO) {
            MetaDataRegisterDTO dto = (MetaDataRegisterDTO) t;
            String fullPath = dto.getRpcType() + "://" + dto.getHost() + ":" + dto.getPort() + dto.getPath();
            addToFail(new Holder(t, fullPath, Constants.META_TYPE));
        }
    }
    
    /**
     * Add failure uri data register.
     *
     * @param <T> the type parameter
     * @param t   the t
     */
    protected <T> void addFailureUriDataRegister(final T t) {
        if (t instanceof URIRegisterDTO) {
            URIRegisterDTO dto = (URIRegisterDTO) t;
            String address = String.join(":", dto.getHost(), String.valueOf(dto.getPort()), dto.getRpcType());
            addToFail(new Holder(t, address, Constants.URI));
        }
    }
    
    private <T> void addToFail(final Holder t) {
        Holder oldObj = concurrentHashMap.get(t.getKey());
        if (oldObj != null) {
            return;
        }
        FailureRegistryTask registryTask = new FailureRegistryTask(t.getKey(), this);
        concurrentHashMap.put(t.getKey(), t);
        timer.add(registryTask);
        logger.warn("Add to failback and wait for execution, {}", t.getPath());
    }
    
    /**
     * Remove.
     *
     * @param key the key
     */
    public void remove(final String key) {
        concurrentHashMap.remove(key);
    }
    
    /**
     * Accpet.
     *
     * @param key the key
     */
    public void accept(final String key) {
        Holder holder = concurrentHashMap.get(key);
        if (holder == null) {
            return;
        }
        String type = holder.getType();
        switch (type) {
            case Constants.URI:
                this.doPersistURI((URIRegisterDTO) holder.getObj());
                break;
            case Constants.META_TYPE:
                this.doPersistInterface((MetaDataRegisterDTO) holder.getObj());
                break;
            default:
                break;
        }
    }
    
    /**
     * Do persist uri.
     *
     * @param registerDTO the register dto
     */
    protected abstract void doPersistURI(URIRegisterDTO registerDTO);
    
    /**
     * Do persist interface.
     *
     * @param registerDTO the register dto
     */
    protected abstract void doPersistInterface(MetaDataRegisterDTO registerDTO);
    
    private static class Holder {
        
        private final Object obj;
        
        private final String path;
        
        private final String type;
        
        /**
         * Instantiates a new Holder.
         *
         * @param obj  the obj
         * @param path the path
         * @param type the type
         */
        Holder(final Object obj, final String path, final String type) {
            this.obj = obj;
            this.path = path;
            this.type = type;
        }
        
        /**
         * Gets obj.
         *
         * @return the obj
         */
        public Object getObj() {
            return obj;
        }
        
        /**
         * Gets path.
         *
         * @return the path
         */
        public String getPath() {
            return path;
        }
        
        /**
         * Gets type.
         *
         * @return the type
         */
        public String getType() {
            return type;
        }
        
        private String getKey() {
            return String.join(":", path, type);
        }
    }
}
