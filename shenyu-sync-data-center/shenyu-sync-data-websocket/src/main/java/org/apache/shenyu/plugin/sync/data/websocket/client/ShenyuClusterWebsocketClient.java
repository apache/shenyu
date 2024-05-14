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
    
    private String masterUrl;
    
    private CountDownLatch countDownLatch;
    
    /**
     * Instantiates a new shenyu websocket client.
     *
     * @param serverUri the server uri
     */
    public ShenyuClusterWebsocketClient(final URI serverUri) {
        super(serverUri);
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
    
    @Override
    public boolean connectBlocking(final long timeout, final TimeUnit timeUnit) throws InterruptedException {
        boolean success = false;
        try {
            success = super.connectBlocking(timeout, timeUnit);
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
    
    /**
     * Now close.
     * now close. will cancel the task execution.
     */
    public void nowClose() {
        this.close();
        if (Objects.nonNull(countDownLatch) && countDownLatch.getCount() > 0) {
            countDownLatch.countDown();
        }
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
