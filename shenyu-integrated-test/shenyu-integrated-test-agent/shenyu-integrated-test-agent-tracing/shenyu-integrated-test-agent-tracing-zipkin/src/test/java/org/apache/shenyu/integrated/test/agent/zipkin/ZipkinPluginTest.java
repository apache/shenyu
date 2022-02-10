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

package org.apache.shenyu.integrated.test.agent.zipkin;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.integrated.test.agent.zipkin.result.ZipkinSpan;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.OrderDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonParser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;


/**
 * Test for tracing zipkin plugin.
 */
public class ZipkinPluginTest extends AbstractPluginDataInit {

    private static final Logger LOG = LoggerFactory.getLogger(ZipkinPluginTest.class);

    private static final String ZIPKIN_HTTP_SERVER = "http://localhost:9411/api/v2/traces?serviceName=shenyu-agent";

    private static final Gson GSON = new Gson();

    @BeforeAll
    public static void setup() throws IOException, InterruptedException {
        OrderDTO user = new OrderDTO("123", "Tom");
        user = HttpHelper.INSTANCE.postGateway("/http/order/save", user, OrderDTO.class);
        assertEquals("hello world save order", user.getName());
        // Sleep for Zipkin to react
        Thread.sleep(5000);
    }

    @Test
    public void testTraces() throws IOException {
        String tracesResponse = Objects.requireNonNull(HttpHelper.INSTANCE.getHttpService(ZIPKIN_HTTP_SERVER, null).body()).string();
        LOG.info("testTraces get response from jaeger: ({})", tracesResponse);
        JsonArray array = JsonParser.parseString(tracesResponse)
                .getAsJsonArray()
                .get(0)
                .getAsJsonArray();
        List<ZipkinSpan> spanList = new ArrayList<>();
        array.forEach(element -> spanList.add(GSON.fromJson(element, ZipkinSpan.class)));
        assertTrue(CollectionUtils.isNotEmpty(spanList));
        spanList.forEach(this::assertSpan);
    }

    private void assertSpan(final ZipkinSpan span) {
        assertNotNull(span);
        assertNotNull(span.getTraceId());
        assertNotNull(span.getId());
        assertNotNull(span.getTimestamp());
        assertNotNull(span.getDuration());
    }

}
