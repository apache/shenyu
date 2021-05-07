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

import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.Message;
import com.google.protobuf.util.JsonFormat;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;

/**
 * MessageWriter.
 *
 * @author zhanglei
 */
@Slf4j
public final class MessageWriter<T extends Message> implements StreamObserver<T> {

    private final JsonFormat.Printer printer;

    private final SoulGrpcResponse results;

    private MessageWriter(final JsonFormat.Printer printer, final SoulGrpcResponse results) {
        this.printer = printer;
        this.results = results;
    }

    /**
     * New instance.
     *
     * @param registry registry
     * @param results  results
     * @param <T>      t
     * @return message message
     */
    public static <T extends Message> MessageWriter<T> newInstance(final JsonFormat.TypeRegistry registry, final SoulGrpcResponse results) {
        return new MessageWriter<>(JsonFormat.printer().usingTypeRegistry(registry), results);
    }

    @Override
    public void onNext(final T value) {
        try {
            results.setResult(printer.print(value));
        } catch (InvalidProtocolBufferException e) {
            log.error("Skipping invalid response message", e);
        }
    }

    @Override
    public void onError(final Throwable t) {
        log.error("Messages write occur errors", t);
    }

    @Override
    public void onCompleted() {
        log.info("Messages write complete");
    }
}
