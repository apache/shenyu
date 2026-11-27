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

package org.apache.shenyu.e2e.testcase.logging.kafka;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class KafkaLogConsumptionTest {

    private static final URI ENDPOINT = URI.create("/http/order/findById?id=23&e2e=current-request");

    @Test
    void ignoresEarlierRequestsWithADifferentQuery() {
        assertTrue(DividePluginCases.findRequestLog(List.of(log("/http/order/findById?id=23&e2e=old-request")), ENDPOINT).isEmpty());
    }

    @Test
    void findsOnlyTheExactPathAndQueryForThisRequest() {
        var records = List.of(
                log("/http/order/findById?id=23&e2e=current-request-more"),
                log("/another/http/order/findById?id=23&e2e=current-request"),
                log(ENDPOINT.toString()));
        var matched = DividePluginCases.findRequestLog(records, ENDPOINT).orElseThrow();
        assertEquals("http://gateway" + ENDPOINT, matched.path("requestUri").asText());
    }

    @Test
    void skipsMalformedRecordsBeforeTheCurrentRequest() {
        var records = List.of(record("not JSON"), log("/invalid uri"), record(null), record(" "), record("null"), record("{}"), log(ENDPOINT.toString()));

        var matched = DividePluginCases.findRequestLog(records, ENDPOINT).orElseThrow();

        assertEquals("http://gateway" + ENDPOINT, matched.path("requestUri").asText());
    }

    private ConsumerRecord<String, String> log(final String endpoint) {
        return record("{\"requestUri\":\"http://gateway" + endpoint + "\"}");
    }

    private ConsumerRecord<String, String> record(final String value) {
        return new ConsumerRecord<>("shenyu-access-logging", 0, 0L, null, value);
    }
}
