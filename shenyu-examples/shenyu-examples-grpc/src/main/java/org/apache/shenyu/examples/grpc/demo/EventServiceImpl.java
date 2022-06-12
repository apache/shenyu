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

package org.apache.shenyu.examples.grpc.demo;


import event.EventRequest;
import event.EventResponse;
import event.EventServiceGrpc;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.client.grpc.common.annotation.ShenyuGrpcClient;
import org.springframework.stereotype.Service;

@ShenyuGrpcClient("/eventService")
@Service
public class EventServiceImpl extends EventServiceGrpc.EventServiceImplBase {

    @ShenyuGrpcClient("/sendEvent")
    @Override
    public void sendEvent(EventRequest request, StreamObserver<EventResponse> responseObserver) {
        EventResponse response = EventResponse.newBuilder().setData("received event:" + request.getData()).build();
        responseObserver.onNext(response);
        responseObserver.onCompleted();
    }

    @ShenyuGrpcClient("/sendEventStream")
    @Override
    public StreamObserver<EventRequest> sendEventStream(StreamObserver<EventResponse> responseObserver) {
        return new StreamObserver<EventRequest>() {
            @Override
            public void onNext(EventRequest request) {
                EventResponse responseData = EventResponse.newBuilder()
                        .setData("received event:" + request.getData())
                        .build();
                responseObserver.onNext(responseData);
            }

            @Override
            public void onError(final Throwable t) {
                t.printStackTrace();
            }

            @Override
            public void onCompleted() {
                EventResponse responseData = EventResponse.newBuilder()
                        .setData("event onCompleted")
                        .build();
                responseObserver.onNext(responseData);
                responseObserver.onCompleted();
            }
        };
    }
}
