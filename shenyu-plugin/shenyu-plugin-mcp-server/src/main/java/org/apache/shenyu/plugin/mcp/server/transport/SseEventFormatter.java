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

package org.apache.shenyu.plugin.mcp.server.transport;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.web.server.ServerWebExchange;

import java.util.Objects;

/**
 * SSE Event Formatter.
 * Handles formatting of SSE events according to the Server-Sent Events
 * specification.
 */
public final class SseEventFormatter {
    
    private static final Logger LOG = LoggerFactory.getLogger(SseEventFormatter.class);
    
    private SseEventFormatter() {
    }
    
    /**
     * Format SSE event according to the Server-Sent Events specification.
     *
     * @param event the SSE event to format
     * @param exchange the server web exchange
     * @return the formatted event as a DataBuffer
     */
    public static DataBuffer formatEvent(final ServerSentEvent<?> event, final ServerWebExchange exchange) {
        StringBuilder sseData = new StringBuilder();
        
        // Add event type if present
        if (Objects.nonNull(event.event())) {
            sseData.append("event: ").append(event.event()).append("\n");
        }
        
        // Add event data if present
        if (Objects.nonNull(event.data())) {
            sseData.append("data: ").append(event.data()).append("\n");
        }
        
        // Add event ID if present
        if (Objects.nonNull(event.id())) {
            sseData.append("id: ").append(event.id()).append("\n");
        }
        
        // Add retry interval if present
        if (Objects.nonNull(event.retry())) {
            sseData.append("retry: ").append(event.retry()).append("\n");
        }
        
        // End of event
        sseData.append("\n");
        
        String formattedEvent = sseData.toString();
        LOG.debug("Formatted SSE event: {}", formattedEvent.trim());
        
        return exchange.getResponse().bufferFactory().wrap(formattedEvent.getBytes());
    }
    
    /**
     * Format SSE comment.
     *
     * @param comment the comment to format
     * @param exchange the server web exchange
     * @return the formatted comment as a DataBuffer
     */
    public static DataBuffer formatComment(final String comment, final ServerWebExchange exchange) {
        String formattedComment = ": " + comment + "\n\n";
        LOG.debug("Formatted SSE comment: {}", formattedComment.trim());
        return exchange.getResponse().bufferFactory().wrap(formattedComment.getBytes());
    }
}