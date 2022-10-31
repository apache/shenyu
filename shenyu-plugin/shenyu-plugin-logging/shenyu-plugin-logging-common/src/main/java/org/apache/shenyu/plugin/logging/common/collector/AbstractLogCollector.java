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
import org.apache.shenyu.plugin.logging.mask.api.matcher.KeyWordMatch;
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

import static org.apache.shenyu.plugin.logging.mask.api.utils.DataMaskUtils.maskForBody;
import static org.apache.shenyu.plugin.logging.mask.api.utils.DataMaskUtils.maskForSingleWord;

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
    public void mask(final L logInfo, final KeyWordMatch keyWordMatch, final String dataMaskAlg) {
        this.maskShenyuRequestLog(logInfo, keyWordMatch, dataMaskAlg);
        this.maskLog(logInfo, keyWordMatch, dataMaskAlg);
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

    private void maskShenyuRequestLog(final L logInfo, final KeyWordMatch keyWordMatch, final String dataMaskAlg) {
        logInfo.setClientIp(maskForSingleWord(GenericLoggingConstant.CLIENT_IP, logInfo.getClientIp(), keyWordMatch, dataMaskAlg));
        logInfo.setTimeLocal(maskForSingleWord(GenericLoggingConstant.TIME_LOCAL, logInfo.getTimeLocal(), keyWordMatch, dataMaskAlg));
        logInfo.setMethod(maskForSingleWord(GenericLoggingConstant.METHOD, logInfo.getMethod(), keyWordMatch, dataMaskAlg));
        logInfo.setRequestUri(maskForSingleWord(GenericLoggingConstant.REQUEST_URI, logInfo.getRequestUri(), keyWordMatch, dataMaskAlg));
        logInfo.setResponseContentLength(Integer.valueOf(maskForSingleWord(GenericLoggingConstant.RESPONSE_CONTENT_LENGTH,
                logInfo.getResponseContentLength().toString(), keyWordMatch, dataMaskAlg)));
        logInfo.setRpcType(maskForSingleWord(GenericLoggingConstant.RPC_TYPE, logInfo.getRpcType(), keyWordMatch, dataMaskAlg));
        logInfo.setStatus(Integer.valueOf(maskForSingleWord(GenericLoggingConstant.STATUS, logInfo.getStatus().toString(), keyWordMatch, dataMaskAlg)));
        logInfo.setUpstreamIp(maskForSingleWord(GenericLoggingConstant.UP_STREAM_IP, logInfo.getUpstreamIp(), keyWordMatch, dataMaskAlg));
        logInfo.setUpstreamResponseTime(Long.valueOf(maskForSingleWord(GenericLoggingConstant.UP_STREAM_RESPONSE_TIME,
                logInfo.getUpstreamResponseTime().toString(), keyWordMatch, dataMaskAlg)));
        logInfo.setUserAgent(maskForSingleWord(GenericLoggingConstant.USERAGENT, logInfo.getUserAgent(), keyWordMatch, dataMaskAlg));
        logInfo.setHost(maskForSingleWord(GenericLoggingConstant.HOST, logInfo.getHost(), keyWordMatch, dataMaskAlg));
        logInfo.setModule(maskForSingleWord(GenericLoggingConstant.MODULE, logInfo.getModule(), keyWordMatch, dataMaskAlg));
        logInfo.setTraceId(maskForSingleWord(GenericLoggingConstant.TRACE_ID, logInfo.getTraceId(), keyWordMatch, dataMaskAlg));
        logInfo.setPath(maskForSingleWord(GenericLoggingConstant.PATH, logInfo.getPath(), keyWordMatch, dataMaskAlg));
        logInfo.setRequestHeader(maskForSingleWord(GenericLoggingConstant.REQUEST_HEADER, logInfo.getRequestHeader(), keyWordMatch, dataMaskAlg));
        logInfo.setResponseHeader(maskForSingleWord(GenericLoggingConstant.RESPONSE_HEADER, logInfo.getResponseHeader(),
                keyWordMatch, dataMaskAlg));
        logInfo.setQueryParams(maskForSingleWord(GenericLoggingConstant.QUERY_PARAMS, logInfo.getQueryParams(), keyWordMatch, dataMaskAlg));
        logInfo.setRequestBody(maskForSingleWord(GenericLoggingConstant.REQUEST_BODY, logInfo.getRequestBody(), keyWordMatch, dataMaskAlg));
        logInfo.setResponseBody(maskForSingleWord(GenericLoggingConstant.RESPONSE_BODY, logInfo.getResponseBody(), keyWordMatch, dataMaskAlg));
        logInfo.setRequestHeader(maskForBody(logInfo.getRequestHeader(), keyWordMatch, dataMaskAlg));
        logInfo.setResponseHeader(maskForBody(logInfo.getResponseHeader(), keyWordMatch, dataMaskAlg));
        logInfo.setQueryParams(maskForBody(logInfo.getQueryParams(), keyWordMatch, dataMaskAlg));
        logInfo.setRequestBody(maskForBody(logInfo.getRequestBody(), keyWordMatch, dataMaskAlg));
        logInfo.setResponseBody(maskForBody(logInfo.getResponseBody(), keyWordMatch, dataMaskAlg));
    }

    /**
     * get log consume client.
     *
     * @return log consume client
     */
    protected abstract T getLogConsumeClient();

    /**
     * mask log.
     *
     * @param log log
     * @param keyWordMatch keyWordMathc
     * @param dataMaskAlg data mask algorithm
     */
    protected abstract void maskLog(L log, KeyWordMatch keyWordMatch, String dataMaskAlg);

    @Override
    public void close() throws Exception {
        started.set(false);
        AbstractLogConsumeClient<?, ?> logCollectClient = getLogConsumeClient();
        if (logCollectClient != null) {
            logCollectClient.close();
        }
    }
}
