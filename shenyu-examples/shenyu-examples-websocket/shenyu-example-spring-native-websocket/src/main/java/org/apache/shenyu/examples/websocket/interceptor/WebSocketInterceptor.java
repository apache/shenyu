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

package org.apache.shenyu.examples.websocket.interceptor;

import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.HandshakeInterceptor;

import java.util.Map;

/**
 * MyInterceptor for websocket.
 */
@Component
public class WebSocketInterceptor implements HandshakeInterceptor {

    private static final Logger LOG = LoggerFactory.getLogger(WebSocketInterceptor.class);

    /**
     * Before handshake.
     * @param request request
     * @param response response
     * @param wsHandler websocketHandler
     * @param attributes  attributes
     * @return enable handshake
     * @throws Exception exception
     */
    @Override
    public boolean beforeHandshake(final ServerHttpRequest request, 
                                   final ServerHttpResponse response, 
                                   final WebSocketHandler wsHandler, 
                                   final Map<String, Object> attributes) throws Exception {
        LOG.info("Shake hands.");
        Map<String, String> paramMap = HttpUtil.decodeParamMap(request.getURI().getQuery(), "utf-8");
        String uid = paramMap.get("token");
        if (StrUtil.isNotBlank(uid)) {
            attributes.put("token", uid);
            LOG.info("user token " + uid + " shook hands successfully！");
            return true;
        }
        LOG.info("user login has expired");
        return false;
    }

    /**
     * After shaking hands.
     * @param request  request
     * @param response  response
     * @param wsHandler  websocketHandler
     * @param exception  exception
     */
    @Override
    public void afterHandshake(final ServerHttpRequest request, 
                               final ServerHttpResponse response, 
                               final WebSocketHandler wsHandler, 
                               final Exception exception) {
        LOG.info("Handshake complete");
    }

}
