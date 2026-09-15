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

import org.apache.shenyu.plugin.logging.common.client.AbstractLogConsumeClient;
import org.apache.shenyu.plugin.logging.common.config.GenericGlobalConfig;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.enums.DataDesensitizeEnum;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * The Test Case For AbstractLogCollector.
 */
public class AbstractLogCollectorTest {

    private final AbstractLogCollector<AbstractLogConsumeClient<?, ShenyuRequestLog>, ShenyuRequestLog, GenericGlobalConfig> collector =
            new AbstractLogCollector<>() {
                @Override
                protected AbstractLogConsumeClient<?, ShenyuRequestLog> getLogConsumeClient() {
                    return null;
                }

                @Override
                protected GenericGlobalConfig getLogCollectConfig() {
                    return null;
                }

                @Override
                protected void desensitizeLog(final ShenyuRequestLog log, final KeyWordMatch keyWordMatch, final String desensitizeAlg) {
                }
            };

    @Test
    public void testDesensitizeToleratesNullBoxedNumericFields() {
        // a chunked byte-type response reaches desensitize with responseContentLength,
        // status and upstreamResponseTime unset (LoggingServerHttpResponse passes a null
        // writer for byte media and only sets status once the status code is committed)
        ShenyuRequestLog log = new ShenyuRequestLog();
        log.setClientIp("192.168.1.1");
        KeyWordMatch keyWordMatch = new KeyWordMatch(new HashSet<>(Collections.singletonList(GenericLoggingConstant.CLIENT_IP)));
        assertDoesNotThrow(() -> collector.desensitize(log, keyWordMatch, DataDesensitizeEnum.CHARACTER_REPLACE.getDataDesensitizeAlg()));
        assertNull(log.getResponseContentLength());
        assertNull(log.getStatus());
        assertNull(log.getUpstreamResponseTime());
        assertNotEquals("192.168.1.1", log.getClientIp());
    }

    @Test
    public void testDesensitizePreservesPopulatedNumericFields() {
        ShenyuRequestLog log = new ShenyuRequestLog();
        log.setClientIp("192.168.1.1");
        log.setResponseContentLength(1024);
        log.setStatus(200);
        log.setUpstreamResponseTime(15L);
        Set<String> keyWords = new HashSet<>(Collections.singletonList(GenericLoggingConstant.CLIENT_IP));
        collector.desensitize(log, new KeyWordMatch(keyWords), DataDesensitizeEnum.CHARACTER_REPLACE.getDataDesensitizeAlg());
        assertEquals(1024, log.getResponseContentLength());
        assertEquals(200, log.getStatus());
        assertEquals(15L, log.getUpstreamResponseTime());
    }
}
