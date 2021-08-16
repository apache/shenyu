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

package org.apache.shenyu.examples.grpc.stream;

import io.grpc.stub.StreamObserver;
import org.apache.shenyu.client.grpc.common.annotation.ShenyuGrpcClient;
import org.springframework.stereotype.Service;
import stream.RequestData;
import stream.ResponseData;
import stream.StreamServiceGrpc;

@Service
public class StreamServiceImpl extends StreamServiceGrpc.StreamServiceImplBase {

    @Override
    @ShenyuGrpcClient(path = "/unaryFun", desc = "unaryFun")
    public void unaryFun(final RequestData request, final StreamObserver<ResponseData> responseObserver) {
        System.out.println("unaryFun received：" + request.getText());

        ResponseData responseData = ResponseData.newBuilder()
                                                .setText("unaryFun response: hello gRPC")
                                                .build();
        responseObserver.onNext(responseData);
        responseObserver.onCompleted();
    }

    @Override
    @ShenyuGrpcClient(path = "/serverStreamingFun", desc = "serverStreamingFun")
    public void serverStreamingFun(final RequestData request, final StreamObserver<ResponseData> responseObserver) {
        System.out.println("serverStreamingFun received：" + request.getText());

        for (int i = 0; i < 10; i++) {
            ResponseData responseData = ResponseData.newBuilder()
                                                    .setText("serverStreamingFun response: hello " + i)
                                                    .build();
            responseObserver.onNext(responseData);
        }

        responseObserver.onCompleted();
    }

    @Override
    @ShenyuGrpcClient(path = "/clientStreamingFun", desc = "clientStreamingFun")
    public StreamObserver<RequestData> clientStreamingFun(StreamObserver<ResponseData> responseObserver) {

        return new StreamObserver<RequestData>() {

            private ResponseData.Builder builder = ResponseData.newBuilder();

            @Override
            public void onNext(final RequestData value) {
                System.out.println("clientStreamingFun received: " + value.getText());
            }

            @Override
            public void onError(final Throwable t) {

            }

            @Override
            public void onCompleted() {
                builder.setText("clientStreamingFun onCompleted");
                responseObserver.onNext(builder.build());
                responseObserver.onCompleted();
            }
        };
    }

    @Override
    @ShenyuGrpcClient(path = "/bidiStreamingFun", desc = "bidiStreamingFun")
    public StreamObserver<RequestData> bidiStreamingFun(final StreamObserver<ResponseData> responseObserver) {

        return new StreamObserver<RequestData>() {

            private ResponseData.Builder builder = ResponseData.newBuilder();

            @Override
            public void onNext(RequestData value) {
                System.out.println("bidiStreamingFun received: " + value.getText());
                ResponseData responseData = ResponseData.newBuilder()
                                                        .setText("bidiStreamingFun response: hello")
                                                        .build();
                responseObserver.onNext(responseData);
            }

            @Override
            public void onError(final Throwable t) { t.printStackTrace();
            }

            @Override
            public void onCompleted() {
                builder.setText("bidiStreamingFun onCompleted");
                responseObserver.onNext(builder.build());
                responseObserver.onCompleted();
            }

        };
    }
}
