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

package org.apache.shenyu.integrated.test.tracing;

import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * The Test Case For the tracing plugin of a docker deployment.
 *
 * <p>The client of the test, the gateway container, the http example container and the OTLP
 * collector container take part in every request, so the whole deployment is covered: the trace
 * context of the client is continued by the gateway, the call to the upstream carries the span of
 * the gateway as its parent, the span is accepted by a real collector, the trace id of the request
 * reaches the log of the gateway, and the sampler of the gateway decides about the trace of a
 * request which is forwarded either way.</p>
 *
 * <p>The tracing plugin exposes the trace id through the {@code shenyu-agent-trace-id} exchange
 * attribute, which the request scoped log line of the gateway reports and which the logging plugins
 * write into the access log. The test verifies the attribute through that log line, because the
 * console logging plugin of the deployment does not print the attribute itself.</p>
 *
 * <p>The upstream is the http example, whose {@code /request/header} endpoint echoes the headers it
 * received, which is how the test reads the trace context the upstream really got. The collector is
 * the {@code debug} exporter of the collector image, which reports the ids of every span it
 * accepted, which is how the test reads what the gateway exported.</p>
 */
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TracingGatewayIntegrationTest extends AbstractTest {

    private static final String GATEWAY_ENDPOINT = "http://localhost:9195";

    private static final String ROUTE = "/http/request/header";

    private static final String HEADER_KEY = "header_key1";

    private static final String TRACEPARENT = "traceparent";

    private static final String TRACESTATE = "tracestate";

    private static final String BAGGAGE = "baggage";

    private static final String SAMPLED = "01";

    private static final String NOT_SAMPLED = "00";

    private static final String SERVICE_NAME = "shenyu-integrated-test-tracing";

    private static final String INSTRUMENTATION_SCOPE = "org.apache.shenyu.plugin.tracing";

    private static final String CHAIN_LOG_MARKER = "shenyu chain handle uri:";

    private static final String SAMPLER_PARENT_BASED = "parent_based";

    private static final String SAMPLER_ALWAYS_ON = "always_on";

    private static final String SAMPLER_ALWAYS_OFF = "always_off";

    private static final String SAMPLER_RATIO = "ratio";

    private static final String CONTINUITY_TRACE_ID = "4bf92f3577b34da6a3ce929d0e0e4736";

    private static final String CONTINUITY_CLIENT_SPAN_ID = "00f067aa0ba902b7";

    private static final String LOG_TRACE_ID = "1af7651916cd43dd8448eb211c80319c";

    private static final String LOG_CLIENT_SPAN_ID = "b7ad6b7169203331";

    private static final String PROPAGATION_TRACE_ID = "0af7651916cd43dd8448eb211c80319c";

    private static final String PROPAGATION_CLIENT_SPAN_ID = "b7ad6b7169203332";

    private static final String UNSAMPLED_TRACE_ID = "5af7651916cd43dd8448eb211c80319c";

    private static final String UNSAMPLED_CLIENT_SPAN_ID = "b7ad6b7169203334";

    private static final String ALWAYS_ON_TRACE_ID = "6af7651916cd43dd8448eb211c80319c";

    private static final String ALWAYS_ON_CLIENT_SPAN_ID = "b7ad6b7169203335";

    private static final String ALWAYS_OFF_TRACE_ID = "7af7651916cd43dd8448eb211c80319c";

    private static final String ALWAYS_OFF_CLIENT_SPAN_ID = "b7ad6b7169203336";

    private static final String UNREACHABLE_TRACE_ID = "9af7651916cd43dd8448eb211c80319c";

    private static final String UNREACHABLE_CLIENT_SPAN_ID = "b7ad6b7169203338";

    private static final Duration AWAIT_TIMEOUT = Duration.ofSeconds(60);

    /**
     * The time the batch processor of the gateway needs to export the spans it has collected. The
     * batch processor exports on a fixed schedule, every five seconds by default, so a request which
     * is not sampled is only proven to be dropped once twice that time has passed.
     */
    private static final Duration EXPORT_SETTLE_TIME = Duration.ofSeconds(12);

    /**
     * The marker the debug exporter of the collector writes before the spans of a resource, the text
     * is the one the debug exporter of the collector 0.111.0 writes.
     */
    private static final String RESOURCE_REPORT_MARKER = "ResourceSpans #";

    private static final Logger LOG = LoggerFactory.getLogger(TracingGatewayIntegrationTest.class);

    private static final HttpClient CLIENT = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();

    private static boolean samplerOverridden;

    /**
     * Waits until the gateway routes the requests of the test.
     *
     * @throws IOException while a request cannot be sent
     * @throws InterruptedException while the wait is interrupted
     */
    @BeforeAll
    public static void awaitRouteReady() throws IOException, InterruptedException {
        final Map<String, String> headers = new HashMap<>(1, 1);
        headers.put(HEADER_KEY, "tracing");
        final long deadline = System.nanoTime() + AWAIT_TIMEOUT.toNanos();
        while (System.nanoTime() < deadline) {
            try {
                final HttpResponse<String> response = get(headers);
                if (200 == response.statusCode() && response.body().contains("response success:")) {
                    return;
                }
            } catch (IOException ex) {
                // the gateway is recreated by the tests of the sampler, so it may not listen yet
                LOG.info("the gateway does not answer yet: {}", ex.getMessage());
            }
            TimeUnit.MILLISECONDS.sleep(500L);
        }
        fail("the gateway did not route " + ROUTE + " to the http example in time");
    }

    /**
     * Restores the gateway of the compose file, so that the sampler of the deployment is the
     * declared one again.
     *
     * @throws IOException while a command of the deployment cannot be executed
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    @AfterAll
    public static void restoreDeployment() throws IOException, InterruptedException {
        if (samplerOverridden) {
            Deployment.restoreGateway();
        }
    }

    @Test
    @Order(1)
    public void testTheClientTheGatewayAndTheUpstreamShareOneTrace() throws IOException, InterruptedException {
        final HttpResponse<String> response = get(headers(CONTINUITY_TRACE_ID, CONTINUITY_CLIENT_SPAN_ID, SAMPLED));
        assertEquals(200, response.statusCode());
        final String[] traceparent = upstreamTraceparent(response.body());
        // the trace of the client is the trace of the upstream call, while the gateway continued it
        // with a span of its own instead of forwarding the span of the client
        assertEquals(CONTINUITY_TRACE_ID, traceparent[1]);
        assertNotEquals(CONTINUITY_CLIENT_SPAN_ID, traceparent[2]);
        assertEquals(SAMPLED, traceparent[3]);
        // the span the gateway exported belongs to that trace, its parent is the span of the client,
        // and its own span is the parent of the call to the upstream: the whole path is one trace
        final String exported = awaitExportedSpan(CONTINUITY_TRACE_ID, CONTINUITY_CLIENT_SPAN_ID, traceparent[2]);
        assertTrue(exported.contains("-> service.name: Str(" + SERVICE_NAME + ")"),
                "the spans have to carry the configured service name");
        assertTrue(exported.contains("InstrumentationScope " + INSTRUMENTATION_SCOPE),
                "the spans have to be reported for the instrumentation scope of the tracing plugin");
    }

    @Test
    @Order(2)
    public void testTheRequestScopedLogReportsTheTraceIdOfTheTrace() throws IOException, InterruptedException {
        final HttpResponse<String> response = get(headers(LOG_TRACE_ID, LOG_CLIENT_SPAN_ID, SAMPLED));
        assertEquals(200, response.statusCode());
        assertEquals(LOG_TRACE_ID, upstreamTraceparent(response.body())[1]);
        // the id of the log is the id the tracing plugin put into the exchange, which is the id of
        // the trace the upstream continued, so a request is searchable by the trace of its log
        awaitGatewayLog(CHAIN_LOG_MARKER + ROUTE + ", traceId:" + LOG_TRACE_ID);
    }

    @Test
    @Order(3)
    public void testTheTraceStateAndTheBaggageReachTheUpstream() throws IOException, InterruptedException {
        final Map<String, String> headers = headers(PROPAGATION_TRACE_ID, PROPAGATION_CLIENT_SPAN_ID, SAMPLED);
        headers.put(TRACESTATE, "vendor=value");
        headers.put(BAGGAGE, "userId=alice");
        final HttpResponse<String> response = get(headers);
        assertEquals(200, response.statusCode());
        final String[] traceparent = upstreamTraceparent(response.body());
        assertEquals(PROPAGATION_TRACE_ID, traceparent[1]);
        assertEquals("vendor=value", echoedHeader(response.body(), TRACESTATE));
        assertEquals("userId=alice", echoedHeader(response.body(), BAGGAGE));
        // the trace state is part of the span as well, the upstream and the collector see the same
        final String exported = awaitExportedSpan(PROPAGATION_TRACE_ID, PROPAGATION_CLIENT_SPAN_ID, traceparent[2]);
        assertTrue(Pattern.compile("TraceState\\s*:\\s*vendor=value").matcher(exported).find(),
                "the span has to carry the trace state of the client");
    }

    @Test
    @Order(4)
    public void testTheParentBasedSamplerDoesNotTraceAnUnsampledClient() throws IOException, InterruptedException {
        // the parent based sampler continues the decision of the client, so the request of a client
        // which did not sample is forwarded with its own decision and its context is not exported
        final HttpResponse<String> response = get(headers(UNSAMPLED_TRACE_ID, UNSAMPLED_CLIENT_SPAN_ID, NOT_SAMPLED));
        assertEquals(200, response.statusCode());
        final String[] traceparent = upstreamTraceparent(response.body());
        assertEquals(UNSAMPLED_TRACE_ID, traceparent[1]);
        assertEquals(NOT_SAMPLED, traceparent[3]);
        // the gateway still continues the trace with a span of its own, dropping the export of the
        // span must not degrade the propagation to a plain copy of the context of the client
        assertNotEquals(UNSAMPLED_CLIENT_SPAN_ID, traceparent[2]);
        assertNotExported(UNSAMPLED_TRACE_ID);
    }

    @Test
    @Order(5)
    public void testTheRequestIsForwardedWhileTheCollectorIsUnreachable() throws IOException, InterruptedException {
        // the collector of the compose file is the exporter of the gateway, once it is stopped the
        // export of the spans fails, which must not affect the forwarding of the request
        Deployment.stopCollector();
        try {
            TimeUnit.SECONDS.sleep(2L);
            final HttpResponse<String> response = get(headers(UNREACHABLE_TRACE_ID, UNREACHABLE_CLIENT_SPAN_ID, SAMPLED));
            assertEquals(200, response.statusCode());
            assertTrue(response.body().contains("response success:"), "the upstream has to answer the request");
            final String[] traceparent = upstreamTraceparent(response.body());
            assertEquals(UNREACHABLE_TRACE_ID, traceparent[1]);
            assertEquals(SAMPLED, traceparent[3]);
        } finally {
            Deployment.startCollector();
        }
    }

    @Test
    @Order(6)
    public void testTheAlwaysOnSamplerTracesAnUnsampledClient() throws IOException, InterruptedException {
        // the sampler of the gateway overrides the decision of the client, so the request is traced
        // and exported although the client did not sample it
        samplerOverridden = true;
        Deployment.restartGatewayWithSampler(SAMPLER_ALWAYS_ON);
        awaitRouteReady();
        awaitGatewayLog("sampler AlwaysOnSampler");
        // the sampler of the gateway overrides the decision of the client, so the request is traced
        // and exported although the client did not sample it
        final HttpResponse<String> response = get(headers(ALWAYS_ON_TRACE_ID, ALWAYS_ON_CLIENT_SPAN_ID, NOT_SAMPLED));
        assertEquals(200, response.statusCode());
        final String[] traceparent = upstreamTraceparent(response.body());
        assertEquals(ALWAYS_ON_TRACE_ID, traceparent[1]);
        assertEquals(SAMPLED, traceparent[3]);
        assertNotEquals(ALWAYS_ON_CLIENT_SPAN_ID, traceparent[2]);
        awaitExportedSpan(ALWAYS_ON_TRACE_ID, ALWAYS_ON_CLIENT_SPAN_ID, traceparent[2]);
    }

    @Test
    @Order(7)
    public void testTheAlwaysOffSamplerDoesNotTraceASampledClient() throws IOException, InterruptedException {
        // the sampler of the gateway overrides the decision of the client again, the request is
        // forwarded but nothing is traced or exported
        samplerOverridden = true;
        Deployment.restartGatewayWithSampler(SAMPLER_ALWAYS_OFF);
        awaitRouteReady();
        awaitGatewayLog("sampler AlwaysOffSampler");
        // the sampler of the gateway overrides the decision of the client again, the request is
        // forwarded but nothing is traced or exported
        final HttpResponse<String> response = get(headers(ALWAYS_OFF_TRACE_ID, ALWAYS_OFF_CLIENT_SPAN_ID, SAMPLED));
        assertEquals(200, response.statusCode());
        final String[] traceparent = upstreamTraceparent(response.body());
        assertEquals(ALWAYS_OFF_TRACE_ID, traceparent[1]);
        assertEquals(NOT_SAMPLED, traceparent[3]);
        // the gateway still starts a span of its own for the request, dropping the export must not
        // degrade the propagation to a plain copy of the context of the client
        assertNotEquals(ALWAYS_OFF_CLIENT_SPAN_ID, traceparent[2]);
        assertNotExported(ALWAYS_OFF_TRACE_ID);
    }

    @Test
    @Order(8)
    public void testTheRatioSamplerHonoursTheSampleRatio() throws IOException, InterruptedException {
        // the log of the starter reports the sampler which was really built from the configuration,
        // the description of the sdk is only matched up to its root sampler
        samplerOverridden = true;
        Deployment.restartGatewayWithSampler(SAMPLER_RATIO, 0.0D);
        awaitRouteReady();
        awaitGatewayLog("sampler ParentBased{root:TraceIdRatioBased{");
        // the ratio decides about the requests which carry no decision of a client, so a ratio of
        // zero drops the trace of a request which comes without a trace context, while the request
        // is forwarded with a context the gateway sampled no
        final Map<String, String> headers = new HashMap<>(1, 1);
        headers.put(HEADER_KEY, "tracing");
        final HttpResponse<String> response = get(headers);
        assertEquals(200, response.statusCode());
        final String[] traceparent = upstreamTraceparent(response.body());
        assertEquals(NOT_SAMPLED, traceparent[3]);
        assertNotExported(traceparent[1]);
    }

    private static Map<String, String> headers(final String traceId, final String spanId, final String sampled) {
        final Map<String, String> headers = new HashMap<>(4, 1);
        headers.put(HEADER_KEY, "tracing");
        headers.put(TRACEPARENT, "00-" + traceId + "-" + spanId + "-" + sampled);
        return headers;
    }

    private static HttpResponse<String> get(final Map<String, String> headers) throws IOException, InterruptedException {
        final HttpRequest.Builder builder = HttpRequest.newBuilder(URI.create(GATEWAY_ENDPOINT + ROUTE)).GET();
        headers.forEach(builder::header);
        return CLIENT.send(builder.build(), HttpResponse.BodyHandlers.ofString());
    }

    /**
     * Acquires the trace context which the upstream received, from the headers the example echoes.
     *
     * @param body the body of the response of the upstream
     * @return the traceparent the upstream received, split into its four fields
     */
    private static String[] upstreamTraceparent(final String body) {
        final String traceparent = echoedHeader(body, TRACEPARENT);
        assertNotNull(traceparent, "the upstream did not receive a traceparent");
        assertFalse(traceparent.contains(","),
                "the upstream received more than one traceparent: " + traceparent);
        final String[] parts = traceparent.split("-");
        assertEquals(4, parts.length, "the upstream received a malformed traceparent " + traceparent);
        assertEquals("00", parts[0]);
        return parts;
    }

    /**
     * Acquires the value of a header from the body of the response of the upstream.
     *
     * <p>The example echoes the headers it received with the form of the {@code HttpHeaders} of the
     * framework, which may change between the releases, so the value is read by its delimiter rather
     * than by the exact format of the map. The value ends at the end of the entry and not at a comma,
     * so the values of a header which the upstream received more than once are returned together,
     * which lets the caller reject a duplicated header.</p>
     *
     * @param body the body of the response of the upstream
     * @param name the name of the header
     * @return the value of the header, null while the upstream did not receive it
     */
    private static String echoedHeader(final String body, final String name) {
        final Matcher matcher = Pattern.compile(Pattern.quote(name) + "\\s*[:=]\\s*\\[?\"?", Pattern.CASE_INSENSITIVE)
                .matcher(body);
        if (!matcher.find()) {
            return null;
        }
        int end = matcher.end();
        while (end < body.length() && '"' != body.charAt(end) && ']' != body.charAt(end)) {
            end++;
        }
        return body.substring(matcher.end(), end).trim();
    }

    /**
     * Waits until the collector reports a span of the given trace whose parent is the span of the
     * client and whose own id is the one the upstream received.
     *
     * @param traceId the trace id of the request
     * @param parentSpanId the span id of the client
     * @param spanId the span id of the gateway, which the upstream received
     * @return the report of the collector about the resource of the span and its scope
     * @throws IOException while the log cannot be read
     * @throws InterruptedException while the wait is interrupted
     */
    private static String awaitExportedSpan(final String traceId, final String parentSpanId, final String spanId)
            throws IOException, InterruptedException {
        final Pattern span = Pattern.compile("Trace ID\\s*:\\s*" + traceId
                + "[\\s\\S]*?Parent ID\\s*:\\s*" + parentSpanId
                + "[\\s\\S]*?^\\s+ID\\s*:\\s*" + spanId, Pattern.MULTILINE);
        final long deadline = System.nanoTime() + AWAIT_TIMEOUT.toNanos();
        while (System.nanoTime() < deadline) {
            final String logs = Deployment.collectorLogs();
            final Matcher matcher = span.matcher(logs);
            if (matcher.find()) {
                return resourceReport(logs, matcher.start());
            }
            TimeUnit.MILLISECONDS.sleep(500L);
        }
        fail("the collector did not accept the span " + spanId + " of the trace " + traceId
                + " whose parent is " + parentSpanId);
        return "";
    }

    /**
     * Narrows the log to the report of one resource, so that the assertions about the resource and
     * the scope belong to the span which was found instead of any other span of the log.
     *
     * @param logs the log of the collector
     * @param position the position of the span in the log
     * @return the report of the resource which contains the span
     */
    private static String resourceReport(final String logs, final int position) {
        final int start = logs.lastIndexOf(RESOURCE_REPORT_MARKER, position);
        // a report which cannot be delimited would turn the assertions of the caller into a check of
        // the whole log, so the marker is required instead
        assertTrue(start >= 0, "the log of the collector does not report a resource before the span");
        final int end = logs.indexOf(RESOURCE_REPORT_MARKER, position);
        return logs.substring(start, end < 0 ? logs.length() : end);
    }

    /**
     * Verifies that the collector accepts no span of the given trace.
     *
     * @param traceId the trace id of the request
     * @throws IOException while the log cannot be read
     * @throws InterruptedException while the wait is interrupted
     */
    private static void assertNotExported(final String traceId) throws IOException, InterruptedException {
        TimeUnit.MILLISECONDS.sleep(EXPORT_SETTLE_TIME.toMillis());
        final String logs = Deployment.collectorLogs();
        assertFalse(Pattern.compile("Trace ID\\s*:\\s*" + traceId).matcher(logs).find(),
                "the sampler has to drop the trace " + traceId + " instead of exporting it");
    }

    private static String awaitGatewayLog(final String marker) throws IOException, InterruptedException {
        final long deadline = System.nanoTime() + AWAIT_TIMEOUT.toNanos();
        String logs = "";
        while (System.nanoTime() < deadline) {
            logs = Deployment.gatewayLogs();
            if (logs.contains(marker)) {
                return logs;
            }
            TimeUnit.MILLISECONDS.sleep(500L);
        }
        fail("the log of the gateway container does not contain " + marker);
        return logs;
    }
}
