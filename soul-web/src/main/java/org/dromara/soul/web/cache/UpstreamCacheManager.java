/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.cache;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.LogUtils;
import org.dromara.soul.common.utils.UrlUtils;
import org.dromara.soul.web.config.SoulConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * this is divide  http url upstream.
 *
 * @author xiaoyu
 */
@Component
public class UpstreamCacheManager {

    private static final Logger LOGGER = LoggerFactory.getLogger(UpstreamCacheManager.class);

    private static final BlockingQueue<SelectorData> BLOCKING_QUEUE = new LinkedBlockingQueue<>(1024);

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    private static final Map<String, List<DivideUpstream>> SCHEDULED_MAP = Maps.newConcurrentMap();

    private static final Integer DELAY_INIT = 30;

    private final SoulConfig soulConfig;

    @Autowired(required = false)
    public UpstreamCacheManager(final SoulConfig soulConfig) {
        this.soulConfig = soulConfig;
    }

    /**
     * Find upstream list by selector id list.
     *
     * @param selectorId the selector id
     * @return the list
     */
    public List<DivideUpstream> findUpstreamListBySelectorId(final String selectorId) {
        return UPSTREAM_MAP.get(selectorId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    static void removeByKey(final String key) {
        SCHEDULED_MAP.remove(key);
        UPSTREAM_MAP.remove(key);
    }

    /**
     * Init.
     */
    @PostConstruct
    public void init() {
        new ThreadPoolExecutor(1, 1,
                0L, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<>(),
                SoulThreadFactory.create("save-upstream-task", false))
                .execute(new Worker());
        new ScheduledThreadPoolExecutor(1,
                SoulThreadFactory.create("scheduled-upstream-task", false))
                .scheduleWithFixedDelay(this::scheduled,
                        DELAY_INIT, soulConfig.getUpstreamScheduledTime(),
                        TimeUnit.SECONDS);
    }

    /**
     * Submit.
     *
     * @param selectorData the selector data
     */
    static void submit(final SelectorData selectorData) {
        try {
            BLOCKING_QUEUE.put(selectorData);
        } catch (InterruptedException e) {
            LOGGER.error(e.getMessage());
        }
    }


    /**
     * Clear.
     */
    static void clear() {
        SCHEDULED_MAP.clear();
        UPSTREAM_MAP.clear();
    }


    /**
     * Execute.
     *
     * @param selectorData the selector data
     */
    public void execute(final SelectorData selectorData) {
        final List<DivideUpstream> upstreamList =
                GsonUtils.getInstance().fromList(selectorData.getHandle(), DivideUpstream.class);
        if (CollectionUtils.isNotEmpty(upstreamList)) {
            SCHEDULED_MAP.put(selectorData.getId(), upstreamList);
            UPSTREAM_MAP.put(selectorData.getId(), check(upstreamList));
        }
    }

    private void scheduled() {
        if (SCHEDULED_MAP.size() > 0) {
            SCHEDULED_MAP.forEach((k, v) -> UPSTREAM_MAP.put(k, check(v)));
        }
    }

    private List<DivideUpstream> check(final List<DivideUpstream> upstreamList) {
        List<DivideUpstream> resultList = Lists.newArrayListWithCapacity(upstreamList.size());
        for (DivideUpstream divideUpstream : upstreamList) {
            final boolean pass = UrlUtils.checkUrl(divideUpstream.getUpstreamUrl());
            if (pass) {
                resultList.add(divideUpstream);
            } else {
                LogUtils.error(LOGGER, "check the url={} is fail ", divideUpstream::getUpstreamUrl);
            }
        }
        return resultList;
    }

    /**
     * The type Worker.
     */
    class Worker implements Runnable {

        @Override
        public void run() {
            runTask();
        }

        private void runTask() {
            for (;;) {
                try {
                    final SelectorData selectorData = BLOCKING_QUEUE.take();
                    Optional.of(selectorData).ifPresent(UpstreamCacheManager.this::execute);
                } catch (InterruptedException e) {
                    LOGGER.warn("BLOCKING_QUEUE take operation was interrupted.", e);
                }
            }
        }
    }

}
