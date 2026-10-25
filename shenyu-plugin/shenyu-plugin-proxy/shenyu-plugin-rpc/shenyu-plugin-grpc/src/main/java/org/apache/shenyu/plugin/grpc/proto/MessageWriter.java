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

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.plugin.grpc.exception.ShenyuGrpcException;
import org.apache.shenyu.protocol.grpc.message.JsonMessage;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
 * own {@code MessageWriter} and {@link ShenyuGrpcResponse}; therefore the mutable counters here
 * need no locking and concurrent RPCs are naturally isolated.</p>
 */
public final class MessageWriter<T extends Message> implements StreamObserver<T> {

    private static final Logger LOG = LoggerFactory.getLogger(MessageWriter.class);

    private static final Gson GSON = new Gson();

    private final ShenyuGrpcResponse grpcResponse;

    private final long maxResultCount;

    private final long maxResultBytes;

    private final AtomicReference<ClientCall<?, ?>> callRef = new AtomicReference<>();

    private long currentCount;

    private long currentBytes;

    private boolean overflowed;

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

    @Override
    public void onNext(final T value) {
        if (overflowed) {
            return;
        }
        String respData = JsonMessage.getDataFromDynamicMessage((DynamicMessage) value);
        long payloadBytes;
        boolean retained;
        if (StringUtils.isNotBlank(respData)) {
            respData = respData.trim();
            if (StringUtils.startsWith(respData, "{") && StringUtils.endsWith(respData, "}")) {
                // standardized json output.
                grpcResponse.getResults().add(GSON.fromJson(respData,
                        new TypeToken<HashMap<String, Object>>() {
                        }.getType()));
                payloadBytes = respData.getBytes(StandardCharsets.UTF_8).length;
                retained = true;
            } else {
                // non-json, non-blank payloads are not retained by the aggregate contract.
                payloadBytes = 0L;
                retained = false;
            }
        } else {
            grpcResponse.getResults().add(respData);
            payloadBytes = 0L;
            retained = true;
        }
        if (!retained) {
            return;
        }
        currentCount++;
        currentBytes += payloadBytes;
        if (exceedsBudget()) {
            triggerOverflow();
        }
    }

    @Override
    public void onError(final Throwable t) {
        LOG.error("Messages write occur errors", t);
    }

    @Override
    public void onCompleted() {
        LOG.info("Messages write complete");
    }

    private boolean exceedsBudget() {
        return (maxResultCount > 0 && currentCount > maxResultCount)
                || (maxResultBytes > 0 && currentBytes > maxResultBytes);
    }

    private void triggerOverflow() {
        overflowed = true;
        String reason = String.format(
                "gRPC aggregated response exceeded budget: retainedCount=%d (max=%d), retainedBytes=%d (max=%d)",
                currentCount, maxResultCount, currentBytes, maxResultBytes);
        // release retained results so the memory can be reclaimed before the RPC terminates.
        grpcResponse.getResults().clear();
        currentBytes = 0L;
        LOG.warn(reason);
        ClientCall<?, ?> call = callRef.get();
        if (Objects.nonNull(call)) {
            call.cancel(reason, new ShenyuGrpcException(reason));
        }
    }
}
