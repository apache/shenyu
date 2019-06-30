/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.admin.listener.http;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.dromara.soul.admin.listener.AbstractDataChangedListener;
import org.dromara.soul.admin.listener.ConfigDataCache;
import org.dromara.soul.admin.listener.DataEventType;
import org.dromara.soul.common.constant.HttpConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.result.SoulResult;
import org.dromara.soul.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;

import javax.servlet.AsyncContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * HTTP long polling, which blocks the client's request thread
 * and informs the client of group information about data changes
 * when there are data changes. If there is no data change after the specified time,
 * the client will make a listening request again.
 * @author huangxiaofeng
 * @date 2019/6/29 22:27
 * @since 2.0.0
 */
public class HttpLongPollingDataChangedListener extends AbstractDataChangedListener {

    private static final Logger logger = LoggerFactory.getLogger(HttpLongPollingDataChangedListener.class);

    public HttpLongPollingDataChangedListener() {

        this.clients = new ArrayBlockingQueue<>(1024);
        this.scheduler = new ScheduledThreadPoolExecutor(1, runnable -> {
            Thread t = new Thread(runnable);
            t.setDaemon(true);
            t.setName("soul-http-long-polling");
            return t;
        });

        // Periodically check the data for changes and update the cache
        scheduler.scheduleWithFixedDelay(() -> {
            this.updateAppAuthCache();
            this.updatePluginCache();
            this.updateRuleCache();
            this.updateSelectorCache();
        }, 300, 300, TimeUnit.SECONDS);

    }

    /**
     * If the configuration data changes, the group information for the change is immediately responded.
     * Otherwise, the client's request thread is blocked until any data changes or the specified timeout is reached.
     */
    public void doLongPolling(HttpServletRequest request, HttpServletResponse response) {

        // compare group md5
        List<ConfigGroupEnum> changedGroup = compareMD5(request);
        String clientIp = getRemoteIp(request);

        // response immediately.
        if (CollectionUtils.isNotEmpty(changedGroup)) {
            this.generateResponse(request, response, changedGroup);
            logger.info("send response with the changed group, ip={}, group={}", clientIp, changedGroup);
            return;
        }

        // listen for config changed.
        final AsyncContext asyncContext = request.startAsync();

        // Asynccontext.settimeout() does not timeout properly, so you have to control it yourself
        asyncContext.setTimeout(0L);

        // block client's thread.
        scheduler.execute(new LongPollingClient(asyncContext, clientIp, HttpConstants.SERVER_MAX_HOLD_TIMEOUT));
    }

    @Override
    protected void afterAppAuthChanged(List<AppAuthData> changed, DataEventType eventType) {
        scheduler.execute(new DataChangeTask(ConfigGroupEnum.APP_AUTH));
    }

    @Override
    protected void afterPluginChanged(List<PluginData> changed, DataEventType eventType) {
        scheduler.execute(new DataChangeTask(ConfigGroupEnum.PLUGIN));
    }

    @Override
    protected void afterRuleChanged(List<RuleData> changed, DataEventType eventType) {
        scheduler.execute(new DataChangeTask(ConfigGroupEnum.RULE));
    }

    @Override
    protected void afterSelectorChanged(List<SelectorData> changed, DataEventType eventType) {
        scheduler.execute(new DataChangeTask(ConfigGroupEnum.SELECTOR));
    }

    /**
     * compare client's md5 and lastModifyTime with server's cache
     * @param request
     * @return
     */
    private static List<ConfigGroupEnum> compareMD5(HttpServletRequest request) {
        List<ConfigGroupEnum> changedGroup = new ArrayList<>(4);
        for (ConfigGroupEnum group : ConfigGroupEnum.values()) {
            // md5,lastModifyTime
            String[] params = StringUtils.split(request.getParameter(group.name()), ',');
            if ( params == null || params.length != 2 ) {
                throw new SoulException("group param invalid:" + request.getParameter(group.name()));
            }
            String clientMd5 = params[0];
            long clientModifyTime = NumberUtils.toLong(params[1]);
            ConfigDataCache serverCache = CACHE.get(group.name());
            if (!StringUtils.equals(clientMd5, serverCache.getMd5()) && clientModifyTime < serverCache.getLastModifyTime()) {
                changedGroup.add(group);
            }
        }
        return changedGroup;
    }

    /**
     * Blocked client.
     */
    private final BlockingQueue<LongPollingClient> clients;

    private final ScheduledExecutorService scheduler;

    /**
     * When a group's data changes, the thread is created to notify the client asynchronously.
     */
    class DataChangeTask implements Runnable {

        /**
         * The Group where the data has changed
         */
        final ConfigGroupEnum groupKey;
        final long changeTime = System.currentTimeMillis();

        DataChangeTask(ConfigGroupEnum groupKey) {
            this.groupKey = groupKey;
        }

        @Override
        public void run() {
            try {
                for (Iterator<LongPollingClient> iter = clients.iterator(); iter.hasNext(); ) {
                    LongPollingClient client = iter.next();
                    iter.remove();
                    client.sendResponse(Collections.singletonList(groupKey));
                    logger.info("send response with the changed group,ip={},group={},changeTime={}", client.ip, groupKey, changeTime);
                }
            } catch (Throwable e) {
                logger.error("data change error.", e);
            }
        }

    }

    /**
     * If you exceed {@link HttpConstants#SERVER_MAX_HOLD_TIMEOUT} and still have no data change,
     * empty data is returned. If the data changes within this time frame, the DataChangeTask
     * cancellations the timed task and responds to the changed group data.
     */
    class LongPollingClient implements Runnable {

        final AsyncContext asyncContext;
        final long createTime;
        final String ip;
        final long timeoutTime;
        Future<?> asyncTimeoutFuture;

        LongPollingClient(AsyncContext ac, String ip, long timeoutTime) {
            this.asyncContext = ac;
            this.createTime = System.currentTimeMillis();
            this.ip = ip;
            this.timeoutTime = timeoutTime;
        }

        @Override
        public void run() {
            this.asyncTimeoutFuture = scheduler.schedule(() -> {
                clients.remove(LongPollingClient.this);
                List<ConfigGroupEnum> changedGroups = HttpLongPollingDataChangedListener.compareMD5((HttpServletRequest) asyncContext.getRequest());
                sendResponse(changedGroups);
            }, timeoutTime, TimeUnit.MILLISECONDS);
            clients.add(this);
        }

        void sendResponse(List<ConfigGroupEnum> changedGroups) {
            // cancel scheduler
            if (null != asyncTimeoutFuture) {
                asyncTimeoutFuture.cancel(false);
            }
            generateResponse((HttpServletRequest)asyncContext.getRequest(), (HttpServletResponse) asyncContext.getResponse(), changedGroups);
            asyncContext.complete();
        }
    }

    /**
     * Send response datagram
     */
    void generateResponse(HttpServletRequest request, HttpServletResponse response, List<ConfigGroupEnum> changedGroups) {
        try {
            String respString = GsonUtils.getInstance().toJson(SoulResult.success("success", changedGroups));
            response.setHeader("Pragma", "no-cache");
            response.setDateHeader("Expires", 0);
            response.setHeader("Cache-Control", "no-cache,no-store");
            response.setContentType(MediaType.APPLICATION_JSON_UTF8_VALUE);
            response.setStatus(HttpServletResponse.SC_OK);
            response.getWriter().println(respString);
        } catch (Exception se) {
            logger.error("Sending response failed.", se);
        }
    }

    private static final String X_REAL_IP = "X-Real-IP";
    private static final String X_FORWARDED_FOR = "X-Forwarded-For";
    private static final String X_FORWARDED_FOR_SPLIT_SYMBOL = ",";

    /**
     * get real client ip
     */
    public static String getRemoteIp(HttpServletRequest request) {
        String xForwardedFor = request.getHeader(X_FORWARDED_FOR);
        if (!StringUtils.isBlank(xForwardedFor)) {
            return xForwardedFor.split(X_FORWARDED_FOR_SPLIT_SYMBOL)[0].trim();
        }
        String nginxHeader = request.getHeader(X_REAL_IP);
        return StringUtils.isBlank(nginxHeader) ? request.getRemoteAddr() : nginxHeader;
    }

}
