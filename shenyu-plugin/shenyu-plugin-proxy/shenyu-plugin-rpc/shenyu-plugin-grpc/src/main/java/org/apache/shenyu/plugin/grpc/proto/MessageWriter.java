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

package org.apache.shenyu.plugin.grpc.proto;

import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics;
import org.apache.shenyu.common.metrics.GrpcAggregationMetrics.Outcome;
import org.apache.shenyu.plugin.grpc.exception.ShenyuGrpcException;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Utf8;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.google.protobuf.DynamicMessage;
import com.google.protobuf.Message;

import io.grpc.ClientCall;
import io.grpc.stub.StreamObserver;

/**
 * MessageWriter.
 *
 * <p>Aggregates the response items of a single gRPC call into {@link ShenyuGrpcResponse}.
 * Optionally bounds the retained aggregate by an item-count budget and a payload-byte budget.
 * When a budget is exhausted the underlying RPC is cancelled, the retained results are released
 * and the call future completes exceptionally, so a partial aggregate is never returned as a
 * success-shaped result.</p>
 *
 * <p>The budgets are opt-in: a non-positive limit means unlimited and preserves the historical
 * behavior. The byte budget measures the UTF-8 payload bytes of the retained items, which is a
 * payload-size bound rather than an exact JVM heap bound.</p>
 *
 * <p>A {@code StreamObserver} receives serialized callbacks for one call, and each call owns its
 * own {@code MessageWriter} and {@link ShenyuGrpcResponse}; concurrent RPCs are naturally isolated.
 * Callbacks for one call are synchronized so a late completion cannot race an overflow.</p>
 */
public final class MessageWriter<T extends Message> implements StreamObserver<T> {

    private static final Logger LOG = LoggerFactory.getLogger(MessageWriter.class);

    private static final Gson GSON = new Gson();

    private static final Type RESPONSE_OBJECT_TYPE = new TypeToken<HashMap<String, Object>>() {
    }.getType();

    private final ShenyuGrpcResponse grpcResponse;

    private final long maxResultCount;

    private final long maxResultBytes;

    private final AtomicReference<ClientCall<?, ?>> callRef = new AtomicReference<>();

    private final CompletableFuture<ShenyuGrpcException> overflowFuture = new CompletableFuture<>();

    private long currentCount;

    private long currentBytes;

    private boolean terminated;

    private MessageWriter(final ShenyuGrpcResponse grpcResponse, final long maxResultCount, final long maxResultBytes) {
        this.grpcResponse = grpcResponse;
        this.maxResultCount = maxResultCount;
        this.maxResultBytes = maxResultBytes;
    }

    /**
     * New instance without any aggregate budget (unlimited), preserving the historical behavior.
     *
     * @param results results
     * @param <T> t
     * @return message message
     */
    public static <T extends Message> MessageWriter<T> newInstance(final ShenyuGrpcResponse results) {
        return new MessageWriter<>(results, 0L, 0L);
    }

    /**
     * New instance with an aggregate item-count and payload-byte budget.
     *
     * <p>A non-positive limit means unlimited for that dimension.</p>
     *
     * @param results results
     * @param maxResultCount maximum retained item count, non-positive means unlimited
     * @param maxResultBytes maximum retained payload bytes, non-positive means unlimited
     * @param <T> t
     * @return message message
     */
    public static <T extends Message> MessageWriter<T> newInstance(final ShenyuGrpcResponse results,
                                                                   final long maxResultCount,
                                                                   final long maxResultBytes) {
        return new MessageWriter<>(results, maxResultCount, maxResultBytes);
    }

    /**
     * Bind the underlying call so that it can be cancelled when a budget is exhausted.
     *
     * @param call the client call
     */
    public void bindCall(final ClientCall<?, ?> call) {
        this.callRef.set(call);
    }

    /**
     * Returns the classified failure when this call exceeds an aggregate budget.
     *
     * <p>The future is completed before the underlying transport is cancelled. This lets the
     * owning client fail the result deterministically even when the transport does not synchronously
     * deliver a close callback.</p>
     *
     * @return overflow future
     */
    public CompletableFuture<ShenyuGrpcException> getOverflowFuture() {
        return overflowFuture;
    }

    /**
     * Stops aggregation and releases retained results before the transport closes.
     */
    public synchronized void discardResults() {
        terminated = true;
        grpcResponse.getResults().clear();
    }

    /**
     * Record the admitted aggregate once the call has reached its terminal outcome.
     *
     * @param outcome outcome selected by the call result
     */
    public synchronized void recordMetrics(final Outcome outcome) {
        GrpcAggregationMetrics.record(outcome, currentCount, currentBytes);
    }

    @Override
    public synchronized void onNext(final T value) {
        if (terminated) {
            return;
        }
        String payload = JsonMessage.getDataFromDynamicMessage((DynamicMessage) value);
        if (StringUtils.isBlank(payload)) {
            appendWithinBudget(payload, 0, payload.length(), true);
            return;
        }
        appendJson(payload);
    }

    private void appendJson(final String payload) {
        int start = 0;
        int end = payload.length();
        // Use the same delimiters as String.trim() without copying a payload that may be rejected.
        while (start < end && payload.charAt(start) <= ' ') {
            start++;
        }
        while (start < end && payload.charAt(end - 1) <= ' ') {
            end--;
        }
        if (start == end || payload.charAt(start) != '{' || payload.charAt(end - 1) != '}') {
            return;
        }
        appendWithinBudget(payload, start, end, false);
    }

    private void appendWithinBudget(final String payload, final int start, final int end, final boolean blank) {
        // Characters skipped by the trim bounds each occupy one UTF-8 byte.
        long payloadBytes = utf8Length(payload) - start - (payload.length() - end);
        if (wouldExceedCount() || wouldExceedBytes(payloadBytes)) {
            triggerOverflow(payloadBytes);
            return;
        }
        Object result = blank ? payload : GSON.fromJson(payload.substring(start, end), RESPONSE_OBJECT_TYPE);
        grpcResponse.getResults().add(result);
        currentCount++;
        currentBytes += payloadBytes;
    }

    private static int utf8Length(final String payload) {
        try {
            return Utf8.encodedLength(payload);
        } catch (IllegalArgumentException ignored) {
            // Keep the JDK replacement semantics for malformed UTF-16 strings constructed in-process.
            return payload.getBytes(StandardCharsets.UTF_8).length;
        }
    }

    @Override
    public synchronized void onError(final Throwable t) {
        if (terminated) {
            return;
        }
        discardResults();
        LOG.error("Messages write occur errors", t);
    }

    @Override
    public synchronized void onCompleted() {
        if (terminated) {
            return;
        }
        terminated = true;
        LOG.info("Messages write complete");
    }

    private boolean wouldExceedCount() {
        return maxResultCount > 0 && currentCount >= maxResultCount;
    }

    private boolean wouldExceedBytes(final long payloadBytes) {
        return maxResultBytes > 0 && payloadBytes > maxResultBytes - currentBytes;
    }

    private void triggerOverflow(final long payloadBytes) {
        String reason = String.format(
                "gRPC aggregated response exceeded budget: retainedCount=%d (max=%d), retainedBytes=%d (max=%d), rejectedBytes=%d",
                currentCount, maxResultCount, currentBytes, maxResultBytes, payloadBytes);
        discardResults();
        ShenyuGrpcException failure = new ShenyuGrpcException(reason);
        overflowFuture.complete(failure);
        LOG.warn(reason);
        ClientCall<?, ?> call = callRef.get();
        if (Objects.nonNull(call)) {
            call.cancel(reason, failure);
        }
    }
}
