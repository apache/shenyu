package org.dromara.soul.web.cache;

import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.dromara.soul.common.dto.convert.DivideHandle;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.dto.zk.RuleZkDTO;
import org.dromara.soul.common.utils.GSONUtils;
import org.dromara.soul.common.utils.UrlUtils;
import org.dromara.soul.web.concurrent.SoulThreadFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * this is divide  http url upstream.
 *
 * @author xiaoyu
 */
@Component
@SuppressWarnings("all")
public class UpstreamCacheManager {

    private static final Logger LOGGER = LoggerFactory.getLogger(UpstreamCacheManager.class);

    private static final BlockingQueue<RuleZkDTO> BLOCKING_QUEUE = new LinkedBlockingQueue<>(1024);

    private static final int MAX_THREAD = Runtime.getRuntime().availableProcessors() << 1;

    private static final Map<String, List<DivideUpstream>> UPSTREAM_MAP = Maps.newConcurrentMap();

    /**
     * acquire DivideUpstream list by ruleId.
     *
     * @param ruleId ruleId
     * @return DivideUpstream list {@linkplain  DivideUpstream}
     */
    public List<DivideUpstream> findUpstreamListByRuleId(final String ruleId) {
        return UPSTREAM_MAP.get(ruleId);
    }

    /**
     * Remove by key.
     *
     * @param key the key
     */
    protected static void removeByKey(final String key) {
        UPSTREAM_MAP.remove(key);
    }

    /**
     * Init.
     */
    @PostConstruct
    public void init() {
        synchronized (LOGGER) {
            ExecutorService executorService = new ThreadPoolExecutor(MAX_THREAD, MAX_THREAD,
                    0L, TimeUnit.MILLISECONDS,
                    new LinkedBlockingQueue<>(),
                    SoulThreadFactory.create("divide-upstream-task",
                            false));
            for (int i = 0; i < MAX_THREAD; i++) {
                executorService.execute(new Worker());
            }
        }
    }

    /**
     * Submit.
     *
     * @param ruleZkDTO the rule zk dto
     */
    public static void submit(final RuleZkDTO ruleZkDTO) {
        try {
            BLOCKING_QUEUE.put(ruleZkDTO);
        } catch (InterruptedException e) {
            LOGGER.error(e.getMessage());
        }
    }

    /**
     * Execute.
     *
     * @param ruleZkDTO the rule zk dto
     */
    public void execute(final RuleZkDTO ruleZkDTO) {
        final DivideHandle divideHandle =
                GSONUtils.getInstance().fromJson(ruleZkDTO.getHandle(), DivideHandle.class);
        if (Objects.nonNull(divideHandle)
                && divideHandle.getUpstreamList().size() > 1) {
            final List<DivideUpstream> upstreamList = divideHandle.getUpstreamList();
            for (DivideUpstream divideUpstream : upstreamList) {
                final boolean pass =UrlUtils.checkUrl(divideUpstream.getUpstreamUrl());
                if (!pass) {
                    if (UPSTREAM_MAP.containsKey(ruleZkDTO.getId())) {
                        UPSTREAM_MAP.get(ruleZkDTO.getId())
                                .removeIf(d -> d.getUpstreamUrl().equals(divideUpstream.getUpstreamUrl()));
                    }
                } else {
                    if (UPSTREAM_MAP.containsKey(ruleZkDTO.getId())) {
                        final List<DivideUpstream> divideUpstreams = UPSTREAM_MAP.get(ruleZkDTO.getId());
                        if (CollectionUtils.isNotEmpty(divideUpstreams)) {
                            if (!divideUpstreams.contains(divideUpstream)) {
                                divideUpstreams.add(divideUpstream);
                                UPSTREAM_MAP.put(ruleZkDTO.getId(), divideUpstreams);
                            }
                        }
                    } else {
                        UPSTREAM_MAP.put(ruleZkDTO.getId(), Collections.singletonList(divideUpstream));
                    }
                }
            }
        }
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
            while (true) {
                try {
                    final RuleZkDTO ruleZkDTO = BLOCKING_QUEUE.take();
                    Optional.of(ruleZkDTO).ifPresent(UpstreamCacheManager.this::execute);
                } catch (Exception e) {
                    LOGGER.error(" failure ," + e.getMessage());
                }
            }
        }
    }

}
