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

package org.apache.shenyu.examples.websocket.config;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.socket.WebSocketSession;

import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;

/**
 * WsSessionManager.
 */
@Slf4j
public class WsSessionManager {
    
    private static final Logger LOG = LoggerFactory.getLogger(WsSessionManager.class);
    
    /**
     * save the connection session.
     */
    private static final ConcurrentHashMap<String, WebSocketSession> SESSION_POOL = new ConcurrentHashMap<>();

    /**
     * add session.
     * @param key key
     * @param session session
     */
    public static void add(final String key, final WebSocketSession session) {
        log.info("add session: {}", key);
        SESSION_POOL.put(key, session);
    }

    /**
     * Deleting a session will return the deleted session.
     * @param key key
     * @return the session just removed
     */
    public static WebSocketSession remove(final String key) {
        log.info("remove session: {}", key);
        return SESSION_POOL.remove(key);
    }

    /**
     * Delete and close the connection synchronously.
     * @param key key
     */
    public static void removeAndClose(final String key) {
        WebSocketSession session = remove(key);
        if (session != null) {
            try {
                session.close();
            } catch (IOException e) {
                log.error("session:{} close failed !", key, e);
            }
        }
    }

    /**
     * Get session.
     * @param key key
     * @return session
     */
    public static WebSocketSession get(final String key) {
        return SESSION_POOL.get(key);
    }
}
