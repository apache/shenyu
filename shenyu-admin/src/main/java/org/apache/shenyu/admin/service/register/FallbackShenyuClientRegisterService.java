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

import org.apache.shenyu.common.timer.AbstractRetryTask;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * FallbackShenyuClientRegisterService .
 */
public abstract class FallbackShenyuClientRegisterService implements ShenyuClientRegisterService {

    private final Logger logger = LoggerFactory.getLogger(FallbackShenyuClientRegisterService.class);

    private final Map<String, FallbackHolder> fallsRegisters = new ConcurrentHashMap<>();

    private final Timer timer;

    /**
     * Instantiates a new Fallback shenyu client register service.
     */
    public FallbackShenyuClientRegisterService() {
        timer = WheelTimerFactory.getSharedTimer();
    }

    /**
     * Register uri string.
     *
     * @param selectorName the selector name
     * @param uriList      the uri list
     * @return the string
     */
    @Override
    public String registerURI(final String selectorName, final List<URIRegisterDTO> uriList) {
        String result;
        String key = key(selectorName);
        try {
            this.removeFallBack(key);
            result = this.doRegisterURI(selectorName, uriList);
            logger.info("Register success: {},{}", selectorName, uriList);
        } catch (Exception ex) {
            logger.warn("Register exception: cause:{}", ex.getMessage());
            result = "";
            this.addFallback(key, new FallbackHolder(selectorName, uriList));
        }
        return result;
    }

    private void addFallback(final String key, final FallbackHolder holder) {
        FallbackHolder oldObj = fallsRegisters.get(key);
        if (Objects.nonNull(oldObj)) {
            return;
        }
        FallbackRegisterTask registryTask = new FallbackRegisterTask(key, this);
        fallsRegisters.put(key, holder);
        timer.add(registryTask);
        logger.info("Add to Fallback and wait for execution, {}:{}", holder.getSelectorName(), holder.getUriList());
    }

    private void removeFallBack(final String key) {
        fallsRegisters.remove(key);
    }

    private void recover(final String key) {
        FallbackHolder fallbackHolder = fallsRegisters.get(key);
        if (Objects.nonNull(fallbackHolder)) {
            List<URIRegisterDTO> uriList = fallbackHolder.getUriList();
            String selectorName = fallbackHolder.getSelectorName();
            this.doRegisterURI(selectorName, uriList);
            logger.info("Register success: {},{}", selectorName, uriList);
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

        /**
         * Instantiates a new Fall holder.
         *
         * @param selectorName the selector name
         * @param uriList      the uri list
         */
        FallbackHolder(final String selectorName, final List<URIRegisterDTO> uriList) {
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
    }

    /**
     * The type Fallback register task.
     */
    private static final class FallbackRegisterTask extends AbstractRetryTask {

        private final FallbackShenyuClientRegisterService registerService;

        /**
         * Instantiates a new Abstract retry task.
         *
         * @param key             the key
         * @param registerService the register service
         */
        FallbackRegisterTask(final String key, final FallbackShenyuClientRegisterService registerService) {
            super(key, TimeUnit.SECONDS.toMillis(5), -1);
            this.registerService = registerService;
        }

        /**
         * Do retry.
         *
         * @param key       the key
         * @param timerTask the timer task
         */
        @Override
        protected void doRetry(final String key, final TimerTask timerTask) {
            registerService.recover(key);
            registerService.removeFallBack(key);

        }
    }
}
