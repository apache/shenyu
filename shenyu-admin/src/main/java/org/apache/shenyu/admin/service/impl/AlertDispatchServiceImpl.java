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

package org.apache.shenyu.admin.service.impl;

import com.google.common.collect.Maps;
import com.google.common.util.concurrent.ThreadFactoryBuilder;
import org.apache.shenyu.admin.mapper.AlertReceiverMapper;
import org.apache.shenyu.alert.AlertNotifyHandler;
import org.apache.shenyu.alert.exception.AlertNoticeException;
import org.apache.shenyu.alert.model.AlertContentDTO;
import org.apache.shenyu.admin.service.AlertDispatchService;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.AlertDispatchService}.
 */
@Service
public class AlertDispatchServiceImpl implements AlertDispatchService, InitializingBean {
    
    private static final Logger log = LoggerFactory.getLogger(AlertDispatchServiceImpl.class);
    
    private static final int DISPATCH_THREADS = 3;
    
    private final LinkedBlockingQueue<AlertContentDTO> alertDataQueue;
    
    private final Map<Byte, AlertNotifyHandler> alertNotifyHandlerMap;
    
    private final AlertReceiverMapper alertReceiverMapper;
    
    private final AtomicReference<List<AlertReceiverDTO>> alertReceiverReference;
    
    public AlertDispatchServiceImpl(final List<AlertNotifyHandler> alertNotifyHandlerList, final AlertReceiverMapper alertReceiverMapper) {
        this.alertDataQueue = new LinkedBlockingQueue<>();
        this.alertReceiverMapper = alertReceiverMapper;
        this.alertReceiverReference = new AtomicReference<>();
        alertNotifyHandlerMap = Maps.newHashMapWithExpectedSize(alertNotifyHandlerList.size());
        alertNotifyHandlerList.forEach(r -> alertNotifyHandlerMap.put(r.type(), r));
    }
    
    @Override
    public void afterPropertiesSet() throws Exception {
        ThreadFactory threadFactory = new ThreadFactoryBuilder()
                                              .setUncaughtExceptionHandler((thread, throwable) -> {
                                                  log.error("workerExecutor has uncaughtException.");
                                                  log.error(throwable.getMessage(), throwable);
                                              })
                                              .setDaemon(true)
                                              .setNameFormat("alerter-worker-%d")
                                              .build();
        ThreadPoolExecutor workerExecutor = new ThreadPoolExecutor(3,
                3,
                10,
                TimeUnit.SECONDS,
                new SynchronousQueue<>(),
                threadFactory,
                new ThreadPoolExecutor.AbortPolicy());
        DispatchTask dispatchTask = new DispatchTask();
        for (int i = 0; i < DISPATCH_THREADS; i++) {
            workerExecutor.execute(dispatchTask);
        }
    }
    
    @Override
    public void dispatchAlert(final AlertContentDTO alertContentDTO) {
        alertDataQueue.offer(alertContentDTO);
    }
    
    @Override
    public void clearCache() {
        this.alertReceiverReference.set(null);
    }
    
    private class DispatchTask implements Runnable {
        @Override
        public void run() {
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    AlertContentDTO alert = alertDataQueue.poll();
                    if (alert != null) {
                        sendNotify(alert);
                    }
                } catch (Exception e) {
                    log.error(e.getMessage());
                }
            }
        }
        
        private void sendNotify(final AlertContentDTO alert) {
            // Forward configured email WeChat webhook
            List<AlertReceiverDTO> receivers = matchReceiverByRules(alert);
            for (AlertReceiverDTO receiver : receivers) {
                try {
                    sendNoticeMsg(receiver, alert);
                } catch (AlertNoticeException e) {
                    log.warn("DispatchTask sendNoticeMsg error, message: {}", e.getMessage());
                }
            }
        }
        
        private List<AlertReceiverDTO> matchReceiverByRules(final AlertContentDTO alert) {
            List<AlertReceiverDTO> dtoList = alertReceiverReference.get();
            if (dtoList == null) {
                dtoList = alertReceiverMapper.selectAll();
                alertReceiverReference.set(dtoList);
            }
            return dtoList.stream().filter(item -> {
                if (item.isEnable()) {
                    if (item.isMatchAll()) {
                        return true;
                    }
                    if (item.getLevels() != null && !item.getLevels().isEmpty()) {
                        boolean levelMatch = item.getLevels().stream().anyMatch(level -> level == alert.getLevel());
                        if (!levelMatch) {
                            return false;
                        }
                    }
                    if (item.getLabels() != null && !item.getLabels().isEmpty()) {
                        return item.getLabels().entrySet().stream().anyMatch(entry -> {
                            if (alert.getLabels() == null || !alert.getLabels().containsKey(entry.getKey())) {
                                return false;
                            }
                            String labelValue = alert.getLabels().get(entry.getKey());
                            return Objects.equals(labelValue, entry.getValue());
                        });
                    }
                    return true;
                } else {
                    return false;
                }
            }).collect(Collectors.toList());
        }
        
        private void sendNoticeMsg(final AlertReceiverDTO receiver, final AlertContentDTO alert) {
            if (receiver == null || receiver.getType() == null) {
                log.warn("DispatcherAlarm-sendNoticeMsg params is empty alert:[{}], receiver:[{}]", alert, receiver);
                return;
            }
            byte type = receiver.getType();
            if (alertNotifyHandlerMap.containsKey(type)) {
                alertNotifyHandlerMap.get(type).send(receiver, alert);
            }
        }
    }
}
