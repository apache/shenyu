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

package org.apache.shenyu.plugin.logging.common.collector;

import org.apache.shenyu.common.concurrent.MemorySafeTaskQueue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.concurrent.ShenyuThreadPoolExecutor;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.common.utils.ThreadUtils;
import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.common.utils.LogCollectConfigUtils;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.apache.shenyu.plugin.logging.desensitize.api.utils.DataDesensitizeUtils.desensitizeForBody;
import static org.apache.shenyu.plugin.logging.desensitize.api.utils.DataDesensitizeUtils.desensitizeForSingleWord;

/**
 * abstract log collector,Contains common methods.
 */
public abstract class AbstractLogCollector<T extends AbstractLogConsumeClient<?, L>, L extends ShenyuRequestLog>
        implements LogCollector<L> {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractLogCollector.class);

    private int bufferSize;

    private BlockingQueue<L> bufferQueue;

    private long lastPushTime;

    private final AtomicBoolean started = new AtomicBoolean(true);

    @Override
    public void start() {
        bufferSize = LogCollectConfigUtils.getGenericGlobalConfig().getBufferQueueSize();
        bufferQueue = new LinkedBlockingDeque<>(bufferSize);
        ShenyuConfig config = Optional.ofNullable(Singleton.INST.get(ShenyuConfig.class)).orElse(new ShenyuConfig());
        final ShenyuConfig.SharedPool sharedPool = config.getSharedPool();
        ShenyuThreadPoolExecutor threadExecutor = new ShenyuThreadPoolExecutor(sharedPool.getCorePoolSize(),
                sharedPool.getMaximumPoolSize(), sharedPool.getKeepAliveTime(), TimeUnit.MILLISECONDS,
                new MemorySafeTaskQueue<>(Constants.THE_256_MB),
                ShenyuThreadFactory.create(config.getSharedPool().getPrefix(), true),
                new ThreadPoolExecutor.AbortPolicy());
        started.set(true);
        threadExecutor.execute(this::consume);
    }

    @Override
    public void collect(final L log) {
        if (Objects.isNull(log) || Objects.isNull(getLogConsumeClient())) {
            return;
        }
        if (bufferQueue.size() < bufferSize) {
            bufferQueue.add(log);
        }
    }

    @Override
    public void desensitize(final L logInfo, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
        this.desensitizeShenyuRequestLog(logInfo, keyWordMatch, desensitizeAlg);
        this.desensitizeLog(logInfo, keyWordMatch, desensitizeAlg);
    }

    /**
     * batch and async consume.
     */
    private void consume() {
        while (started.get()) {
            int diffTimeMSForPush = 100;
            try {
                List<L> logs = new ArrayList<>();
                int size = bufferQueue.size();
                long time = System.currentTimeMillis();
                long timeDiffMs = time - lastPushTime;
                int batchSize = 100;
                if (size >= batchSize || timeDiffMs > diffTimeMSForPush) {
                    bufferQueue.drainTo(logs, batchSize);
                    AbstractLogConsumeClient<?, L> logCollectClient = getLogConsumeClient();
                    if (Objects.nonNull(logCollectClient)) {
                        logCollectClient.consume(logs);
                    }
                    lastPushTime = time;
                } else {
                    ThreadUtils.sleep(TimeUnit.MILLISECONDS, diffTimeMSForPush);
                }
            } catch (Exception e) {
                LOG.error("DefaultLogCollector collect log error", e);
                ThreadUtils.sleep(TimeUnit.MILLISECONDS, diffTimeMSForPush);
            }
        }
    }

    private void desensitizeShenyuRequestLog(final L logInfo, final KeyWordMatch keyWordMatch, final String desensitizedAlg) {
        logInfo.setClientIp(desensitizeForSingleWord(GenericLoggingConstant.CLIENT_IP, logInfo.getClientIp(), keyWordMatch, desensitizedAlg));
        logInfo.setTimeLocal(desensitizeForSingleWord(GenericLoggingConstant.TIME_LOCAL, logInfo.getTimeLocal(), keyWordMatch, desensitizedAlg));
        logInfo.setMethod(desensitizeForSingleWord(GenericLoggingConstant.METHOD, logInfo.getMethod(), keyWordMatch, desensitizedAlg));
        logInfo.setRequestUri(desensitizeForSingleWord(GenericLoggingConstant.REQUEST_URI, logInfo.getRequestUri(), keyWordMatch, desensitizedAlg));
        logInfo.setResponseContentLength(Integer.valueOf(desensitizeForSingleWord(GenericLoggingConstant.RESPONSE_CONTENT_LENGTH,
                logInfo.getResponseContentLength().toString(), keyWordMatch, desensitizedAlg)));
        logInfo.setRpcType(desensitizeForSingleWord(GenericLoggingConstant.RPC_TYPE, logInfo.getRpcType(), keyWordMatch, desensitizedAlg));
        logInfo.setStatus(Integer.valueOf(desensitizeForSingleWord(GenericLoggingConstant.STATUS, logInfo.getStatus().toString(), keyWordMatch, desensitizedAlg)));
        logInfo.setUpstreamIp(desensitizeForSingleWord(GenericLoggingConstant.UP_STREAM_IP, logInfo.getUpstreamIp(), keyWordMatch, desensitizedAlg));
        logInfo.setUpstreamResponseTime(Long.valueOf(desensitizeForSingleWord(GenericLoggingConstant.UP_STREAM_RESPONSE_TIME,
                logInfo.getUpstreamResponseTime().toString(), keyWordMatch, desensitizedAlg)));
        logInfo.setUserAgent(desensitizeForSingleWord(GenericLoggingConstant.USERAGENT, logInfo.getUserAgent(), keyWordMatch, desensitizedAlg));
        logInfo.setHost(desensitizeForSingleWord(GenericLoggingConstant.HOST, logInfo.getHost(), keyWordMatch, desensitizedAlg));
        logInfo.setModule(desensitizeForSingleWord(GenericLoggingConstant.MODULE, logInfo.getModule(), keyWordMatch, desensitizedAlg));
        logInfo.setTraceId(desensitizeForSingleWord(GenericLoggingConstant.TRACE_ID, logInfo.getTraceId(), keyWordMatch, desensitizedAlg));
        logInfo.setPath(desensitizeForSingleWord(GenericLoggingConstant.PATH, logInfo.getPath(), keyWordMatch, desensitizedAlg));
        logInfo.setRequestHeader(desensitizeForSingleWord(GenericLoggingConstant.REQUEST_HEADER, logInfo.getRequestHeader(), keyWordMatch, desensitizedAlg));
        logInfo.setResponseHeader(desensitizeForSingleWord(GenericLoggingConstant.RESPONSE_HEADER, logInfo.getResponseHeader(),
                keyWordMatch, desensitizedAlg));
        logInfo.setQueryParams(desensitizeForSingleWord(GenericLoggingConstant.QUERY_PARAMS, logInfo.getQueryParams(), keyWordMatch, desensitizedAlg));
        logInfo.setRequestBody(desensitizeForSingleWord(GenericLoggingConstant.REQUEST_BODY, logInfo.getRequestBody(), keyWordMatch, desensitizedAlg));
        logInfo.setResponseBody(desensitizeForSingleWord(GenericLoggingConstant.RESPONSE_BODY, logInfo.getResponseBody(), keyWordMatch, desensitizedAlg));
        logInfo.setRequestHeader(desensitizeForBody(logInfo.getRequestHeader(), keyWordMatch, desensitizedAlg));
        logInfo.setResponseHeader(desensitizeForBody(logInfo.getResponseHeader(), keyWordMatch, desensitizedAlg));
        logInfo.setQueryParams(desensitizeForBody(logInfo.getQueryParams(), keyWordMatch, desensitizedAlg));
        logInfo.setRequestBody(desensitizeForBody(logInfo.getRequestBody(), keyWordMatch, desensitizedAlg));
        logInfo.setResponseBody(desensitizeForBody(logInfo.getResponseBody(), keyWordMatch, desensitizedAlg));
    }

    /**
     * get log consume client.
     *
     * @return log consume client
     */
    protected abstract T getLogConsumeClient();

    /**
     * desensitize log.
     *
     * @param log log
     * @param keyWordMatch keyWordMathc
     * @param desensitizeAlg data desensitize algorithm
     */
    protected abstract void desensitizeLog(L log, KeyWordMatch keyWordMatch, String desensitizeAlg);

    @Override
    public void close() throws Exception {
        started.set(false);
        AbstractLogConsumeClient<?, ?> logCollectClient = getLogConsumeClient();
        if (logCollectClient != null) {
            logCollectClient.close();
        }
    }
}
