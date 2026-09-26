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

package org.apache.shenyu.admin.listener.websocket;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.publish.InstanceInfoReportEventPublisher;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shenyu.admin.utils.ThreadLocalUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.constant.RunningModeConstants;
import org.apache.shenyu.common.dto.WebsocketSyncFrame;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.RunningModeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.websocket.OnClose;
import jakarta.websocket.OnError;
import jakarta.websocket.OnMessage;
import jakarta.websocket.OnOpen;
import jakarta.websocket.Session;
import jakarta.websocket.server.ServerEndpoint;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArraySet;

/**
 * The type Websocket data changed listener.
 *
 * @since 2.0.0
 */
@ServerEndpoint(value = "/websocket", configurator = WebsocketConfigurator.class)
public class WebsocketCollector {
    
    private static final Logger LOG = LoggerFactory.getLogger(WebsocketCollector.class);
    
    private static final Set<Session> SESSION_SET = new CopyOnWriteArraySet<>();
    
    private static final Map<String, Set<Session>> NAMESPACE_SESSION_MAP = Maps.newConcurrentMap();

    private static final Map<Session, SessionSendQueue> SESSION_SEND_QUEUES = Maps.newConcurrentMap();

    /**
     * Namespace captured at registration. {@code Session#isOpen()} is already false when
     * {@code @OnClose} runs, so the namespace cannot be read from the session at teardown.
     */
    private static final Map<Session, String> SESSION_NAMESPACE_IDS = Maps.newConcurrentMap();
    
    private static final String SESSION_KEY = "sessionKey";

    private static final ThreadLocal<InitialSync> INITIAL_SYNC = new ThreadLocal<>();
    
    /**
     * On open.
     *
     * @param session the session
     */
    @OnOpen
    public void onOpen(final Session session) {
        String clientIp = getClientIp(session);
        LOG.info("websocket on client[{}] open successful, maxTextMessageBufferSize: {}",
                clientIp, session.getMaxTextMessageBufferSize());
        String namespaceId = getNamespaceId(session);
        if (StringUtils.isBlank(namespaceId)) {
            throw new ShenyuException("websocket on client open failed, namespaceId is null");
        }
        SESSION_SET.add(session);
        SESSION_NAMESPACE_IDS.put(session, namespaceId);
        LOG.info("websocket on client[{}] open successful, namespaceId: {}", clientIp, namespaceId);
        NAMESPACE_SESSION_MAP.compute(namespaceId, (id, sessions) -> {
            Set<Session> registered = Objects.isNull(sessions) ? Sets.newConcurrentHashSet() : sessions;
            registered.add(session);
            return registered;
        });
    }
    
    private static String getClientIp(final Session session) {
        if (!session.isOpen()) {
            return StringUtils.EMPTY;
        }
        Map<String, Object> userProperties = session.getUserProperties();
        if (MapUtils.isEmpty(userProperties)) {
            return StringUtils.EMPTY;
        }
        
        return Optional.ofNullable(userProperties.get(WebsocketListener.CLIENT_IP_NAME))
                .map(Object::toString)
                .orElse(StringUtils.EMPTY);
    }

    private static String getClientPort(final Session session) {
        if (!session.isOpen()) {
            return StringUtils.EMPTY;
        }
        Map<String, Object> userProperties = session.getUserProperties();
        if (MapUtils.isEmpty(userProperties)) {
            return StringUtils.EMPTY;
        }

        return Optional.ofNullable(userProperties.get(Constants.CLIENT_PORT_NAME))
                .map(Object::toString)
                .orElse(StringUtils.EMPTY);
    }
    
    private static String getNamespaceId(final Session session) {
        if (!session.isOpen()) {
            LOG.warn("websocket session is closed, can not get namespaceId");
            return null;
        }
        Map<String, Object> userProperties = session.getUserProperties();
        if (MapUtils.isEmpty(userProperties)) {
            LOG.warn("websocket session userProperties is empty, can not get namespaceId");
            return null;
        }
        
        return Optional.ofNullable(userProperties.get(Constants.SHENYU_NAMESPACE_ID))
                .map(Object::toString)
                .orElse(null);
    }
    
    /**
     * On message.
     *
     * @param message the message
     * @param session the session
     */
    @OnMessage
    public void onMessage(final String message, final Session session) {
        if (message.startsWith(WebsocketSyncFrame.REQUEST_PREFIX)) {
            initialSync(message.substring(WebsocketSyncFrame.REQUEST_PREFIX.length()), session);
            return;
        }
        if (!Objects.equals(message, DataEventTypeEnum.MYSELF.name())
                && !Objects.equals(message, DataEventTypeEnum.RUNNING_MODE.name())
                && !message.contains("bootstrapInstanceInfo")) {
            return;
        }
        if (message.contains(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO)) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("bootstrap report instance info: {}", message);
            }
            String namespaceId = getNamespaceId(session);
            Map<String, Object> infoMap = GsonUtils.getInstance().convertToMap(message);
            Object o = infoMap.get(InstanceTypeConstants.BOOTSTRAP_INSTANCE_INFO);
            InstanceInfoReportEvent instanceInfoRegisterDTO = InstanceInfoReportEvent.builder()
                    .instanceIp(getClientIp(session))
                    .instancePort(getClientPort(session))
                    .instanceType(InstanceTypeConstants.BOOTSTRAP_INSTANCE_TYPE)
                    .instanceInfo(GsonUtils.getInstance().toJson(o))
                    .instanceState(1)
                    .namespaceId(namespaceId)
                    .build();
            SpringBeanUtils.getInstance().getBean(InstanceInfoReportEventPublisher.class).publish(instanceInfoRegisterDTO);
            return;
        }
        
        if (Objects.equals(message, DataEventTypeEnum.RUNNING_MODE.name())) {
            if (LOG.isDebugEnabled()) {
                LOG.debug("websocket fetching running mode info...");
            }
            // check if this node is master
            boolean isMaster = true;
            String runningMode = RunningModeEnum.STANDALONE.name();
            String masterUrl = StringUtils.EMPTY;
            ClusterProperties clusterProperties = SpringBeanUtils.getInstance().getBean(ClusterProperties.class);
            if (clusterProperties.isEnabled()) {
                ClusterSelectMasterService clusterSelectMasterService = SpringBeanUtils.getInstance().getBean(ClusterSelectMasterService.class);
                runningMode = RunningModeEnum.CLUSTER.name();
                isMaster = clusterSelectMasterService.isMaster();
                masterUrl = clusterSelectMasterService.getMasterUrl();
            }
            Map<String, Object> map = Maps.newHashMap();
            map.put(RunningModeConstants.EVENT_TYPE, DataEventTypeEnum.RUNNING_MODE.name());
            map.put(RunningModeConstants.IS_MASTER, isMaster);
            map.put(RunningModeConstants.RUNNING_MODE, runningMode);
            map.put(RunningModeConstants.MASTER_URL, masterUrl
                    .replace("http", "ws")
                    .replace("https", "ws")
                    .concat("/websocket"));
            if (isMaster) {
                ThreadLocalUtils.put(SESSION_KEY, session);
            }
            
            sendMessageBySession(session, JsonUtils.toJson(map));
            return;
        }
        
        if (Objects.equals(message, DataEventTypeEnum.MYSELF.name())) {
            try {
                ThreadLocalUtils.put(SESSION_KEY, session);
                String namespaceId = getNamespaceId(session);
                SpringBeanUtils.getInstance().getBean(SyncDataService.class).syncAllByNamespaceId(DataEventTypeEnum.MYSELF, namespaceId);
            } finally {
                ThreadLocalUtils.clear();
            }
        }
        
    }
    
    /**
     * On close.
     *
     * @param session the session
     */
    @OnClose
    public void onClose(final Session session) {
        clearSession(session);
        LOG.warn("websocket close on client[{}]", getClientIp(session));
    }
    
    /**
     * On error.
     *
     * @param session the session
     * @param error the error
     */
    @OnError
    public void onError(final Session session, final Throwable error) {
        clearSession(session);
        LOG.error("websocket collection on client[{}] error: ", getClientIp(session), error);
    }
    
    /**
     * Send.
     *
     * @param message the message
     * @param type the type
     */
    public static void send(final String message, final DataEventTypeEnum type) {
        if (StringUtils.isBlank(message)) {
            return;
        }
        
        if (DataEventTypeEnum.MYSELF == type) {
            Session session = (Session) ThreadLocalUtils.get(SESSION_KEY);
            if (Objects.nonNull(session)) {
                if (session.isOpen()) {
                    sendMessageBySession(session, message);
                } else {
                    removeSessionIndexes(session);
                }
            }
        } else {
            for (Session registered : new ArrayList<>(SESSION_SET)) {
                if (registered.isOpen()) {
                    sendMessageBySession(registered, message);
                } else {
                    removeSessionIndexes(registered);
                }
            }
        }
        
    }
    
    /**
     * Send.
     *
     * @param namespaceId the namespaceId
     * @param message the message
     * @param type the type
     */
    public static void send(final String namespaceId, final String message, final DataEventTypeEnum type) {
        if (StringUtils.isBlank(message)) {
            return;
        }
        if (StringUtils.isBlank(namespaceId)) {
            throw new ShenyuException("namespaceId can not be null");
        }
        LOG.info("websocket send message to namespaceId: {}, message: {}", namespaceId, maskSensitive(message));
        if (DataEventTypeEnum.MYSELF == type) {
            Session session = (Session) ThreadLocalUtils.get(SESSION_KEY);
            if (Objects.nonNull(session)) {
                if (session.isOpen()) {
                    sendMessageBySession(session, message);
                } else {
                    removeSessionIndexes(session);
                }
            }
        } else {
            Set<Session> sessions = NAMESPACE_SESSION_MAP.get(namespaceId);
            if (Objects.isNull(sessions) || sessions.isEmpty()) {
                return;
            }
            for (Session registered : new ArrayList<>(sessions)) {
                if (registered.isOpen()) {
                    sendMessageBySession(registered, message);
                } else {
                    removeSessionIndexes(registered);
                }
            }
        }
        
    }
    
    private void initialSync(final String requestId, final Session session) {
        UUID.fromString(requestId);
        ClusterProperties properties = SpringBeanUtils.getInstance().getBean(ClusterProperties.class);
        if (properties.isEnabled()
                && !SpringBeanUtils.getInstance().getBean(ClusterSelectMasterService.class).isMaster()) {
            return;
        }
        InitialSync sync = new InitialSync(session, requestId);
        try {
            INITIAL_SYNC.set(sync);
            ThreadLocalUtils.put(SESSION_KEY, session);
            boolean success = SpringBeanUtils.getInstance().getBean(SyncDataService.class)
                    .syncAllByNamespaceId(DataEventTypeEnum.MYSELF, getNamespaceId(session));
            if (success && (!properties.isEnabled()
                    || SpringBeanUtils.getInstance().getBean(ClusterSelectMasterService.class).isMaster())) {
                SESSION_SEND_QUEUES.computeIfAbsent(session, SessionSendQueue::new)
                        .send(GsonUtils.getInstance().toJson(new WebsocketSyncFrame(requestId, sync.sequence, null)));
            }
        } finally {
            INITIAL_SYNC.remove();
            ThreadLocalUtils.clear();
        }
    }

    private static void sendMessageBySession(final Session session, final String message) {
        InitialSync sync = INITIAL_SYNC.get();
        if (Objects.nonNull(sync) && sync.session == session) {
            SESSION_SEND_QUEUES.computeIfAbsent(session, SessionSendQueue::new)
                    .send(GsonUtils.getInstance().toJson(new WebsocketSyncFrame(sync.requestId, sync.sequence++, message)));
        } else {
            SESSION_SEND_QUEUES.computeIfAbsent(session, SessionSendQueue::new).send(message);
        }
    }

    private static void removeSessionSendQueue(final Session session) {
        SessionSendQueue sendQueue = SESSION_SEND_QUEUES.remove(session);
        if (Objects.nonNull(sendQueue)) {
            sendQueue.close();
        }
    }
    
    private static void clearSession(final Session session) {
        removeSessionIndexes(session);
        ThreadLocalUtils.clear();
    }

    private static void removeSessionIndexes(final Session session) {
        SESSION_SET.remove(session);
        removeSessionSendQueue(session);
        String namespaceId = SESSION_NAMESPACE_IDS.remove(session);
        if (StringUtils.isNotBlank(namespaceId)) {
            NAMESPACE_SESSION_MAP.compute(namespaceId, (id, sessions) -> {
                if (Objects.isNull(sessions)) {
                    return null;
                }
                sessions.remove(session);
                return sessions.isEmpty() ? null : sessions;
            });
        }
    }
    
    private static String maskSensitive(final String json) {
        if (Objects.isNull(json)) {
            return null;
        }
        try {
            Map<String, Object> map = JsonUtils.jsonToMap(json);
            if (Objects.nonNull(map)) {
                if (map.containsKey("apiKey")) {
                    map.put("apiKey", "******");
                }
                if (map.containsKey("realApiKey")) {
                    map.put("realApiKey", "******");
                }
                return JsonUtils.toJson(map);
            }
            return json;
        } catch (Exception e) {
            return json;
        }
    }

    private static final class InitialSync {

        private final Session session;

        private final String requestId;

        private int sequence;

        private InitialSync(final Session session, final String requestId) {
            this.session = session;
            this.requestId = requestId;
        }
    }

    private static final class SessionSendQueue {

        private final Session session;

        private final Queue<String> messages = new ArrayDeque<>();

        private boolean sending;

        private boolean closed;

        private SessionSendQueue(final Session session) {
            this.session = session;
        }

        private void send(final String message) {
            boolean startSending = false;
            synchronized (this) {
                if (closed) {
                    return;
                }
                messages.offer(message);
                if (!sending) {
                    sending = true;
                    startSending = true;
                }
            }
            if (startSending) {
                sendNext();
            }
        }

        private void sendNext() {
            final String message;
            synchronized (this) {
                if (closed) {
                    sending = false;
                    messages.clear();
                    return;
                }
                message = messages.poll();
                if (Objects.isNull(message)) {
                    sending = false;
                    return;
                }
            }
            try {
                session.getAsyncRemote().sendText(message, result -> {
                    if (!result.isOK()) {
                        LOG.error("websocket send result is exception: ", result.getException());
                    }
                    sendNext();
                });
            } catch (RuntimeException ex) {
                LOG.error("websocket send result is exception: ", ex);
                sendNext();
            }
        }

        private void close() {
            synchronized (this) {
                closed = true;
                messages.clear();
            }
        }
    }
}
