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

package org.apache.shenyu.plugin.sync.data.websocket.client;

import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.timer.AbstractRoundTask;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.apache.shenyu.common.utils.JsonUtils;
import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * The type shenyu websocket client.
 */
public final class ShenyuClusterWebsocketClient extends WebSocketClient {
    
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuClusterWebsocketClient.class);
    
    private final Timer timer;
    
    private TimerTask timerTask;
    
    private String masterUrl;
    
    private CountDownLatch countDownLatch;
    
    /**
     * Instantiates a new shenyu websocket client.
     *
     * @param serverUri the server uri
     */
    public ShenyuClusterWebsocketClient(final URI serverUri) {
        super(serverUri);
        this.timer = WheelTimerFactory.getSharedTimer();
        this.connection();
        countDownLatch = new CountDownLatch(1);
    }
    
    /**
     * Instantiates a new shenyu websocket client.
     *
     * @param serverUri the server uri
     * @param headers the headers
     */
    public ShenyuClusterWebsocketClient(final URI serverUri, final Map<String, String> headers) {
        super(serverUri, headers);
        this.timer = WheelTimerFactory.getSharedTimer();
        this.connection();
        countDownLatch = new CountDownLatch(1);
    }
    
    @Override
    public boolean connectBlocking() {
        boolean success = false;
        try {
            success = super.connectBlocking();
        } catch (Exception exception) {
            LOG.error("websocket connection server[{}] is error.....[{}]", this.getURI().toString(), exception.getMessage());
        }
        if (success) {
            LOG.info("websocket connection server[{}] is successful.....", this.getURI().toString());
        } else {
            LOG.warn("websocket connection server[{}] is error.....", this.getURI().toString());
        }
        return success;
    }
    
    private void connection() {
        this.connectBlocking();
        this.timer.add(timerTask = new AbstractRoundTask(null, TimeUnit.SECONDS.toMillis(10)) {
            @Override
            public void doRun(final String key, final TimerTask timerTask) {
                healthCheck();
            }
        });
    }
    
    @Override
    public void onOpen(final ServerHandshake serverHandshake) {
        send(DataEventTypeEnum.CLUSTER.name());
        countDownLatch = new CountDownLatch(1);
    }
    
    @Override
    public void onMessage(final String result) {
        handleClusterResult(result);
    }
    
    @Override
    public void onClose(final int i, final String s, final boolean b) {
        this.close();
    }
    
    @Override
    public void onError(final Exception e) {
        LOG.error("websocket server[{}] is error.....", getURI(), e);
        if (Objects.nonNull(countDownLatch) && countDownLatch.getCount() > 0) {
            countDownLatch.countDown();
        }
    }
    
    @Override
    public void close() {
        if (this.isOpen()) {
            super.close();
        }
    }
    
    private void healthCheck() {
        try {
            if (!this.isOpen()) {
                this.reconnectBlocking();
            } else {
                this.sendPing();
                LOG.debug("websocket send to [{}] ping message successful", this.getURI());
            }
        } catch (Exception e) {
            LOG.error("websocket connect is error :{}", e.getMessage());
        }
    }
    
    /**
     * Now close.
     * now close. will cancel the task execution.
     */
    public void nowClose() {
        this.close();
        if (Objects.nonNull(countDownLatch) && countDownLatch.getCount() > 0) {
            countDownLatch.countDown();
        }
        timerTask.cancel();
    }
    
    /**
     * get cluster master url.
     * @return cluster master websocket url
     */
    public String getMasterUrl() {
        if (Objects.nonNull(countDownLatch) && countDownLatch.getCount() > 0) {
            try {
                countDownLatch.await();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
        String url = this.masterUrl;
        this.masterUrl = "";
        return url;
    }
    
    /**
     * handle admin cluster message.
     *
     * @param result result
     */
    private void handleClusterResult(final String result) {
        LOG.info("handleResult({})", result);
        Map<String, Object> jsonToMap = JsonUtils.jsonToMap(result);
        Object eventType = jsonToMap.get("eventType");
        if (!Objects.equals("CLUSTER", eventType)) {
            return;
        }
        this.masterUrl = String.valueOf(jsonToMap.get("masterUrl"));
        if (Objects.nonNull(countDownLatch) && countDownLatch.getCount() > 0) {
            countDownLatch.countDown();
            if (!Objects.equals(this.masterUrl, this.getURI().toString())) {
                LOG.info("not connected to master, no close, current url:[{}], master url:[{}]", this.getURI().toString(), this.masterUrl);
                this.nowClose();
            }
        }
    }
}
