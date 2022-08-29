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

import hello.HelloRequest;
import hello.HelloResponse;
import hello.HelloServiceGrpc;

import io.grpc.stub.StreamObserver;
import org.apache.shenyu.client.grpc.common.annotation.ShenyuGrpcClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@ShenyuGrpcClient("/helloService/**")
@Service
public class HelloServiceImpl extends HelloServiceGrpc.HelloServiceImplBase {
    
    private static final Logger LOG = LoggerFactory.getLogger(HelloServiceImpl.class);
    
    @Override
    public void hello(final HelloRequest request, final StreamObserver<HelloResponse> responseObserver) {
        HelloResponse response = HelloResponse.newBuilder().setData("hello: " + request.getData()).build();
        responseObserver.onNext(response);
        responseObserver.onCompleted();
    }

    @Override
    public StreamObserver<HelloRequest> helloEveryOne(final StreamObserver<HelloResponse> responseObserver) {
        return new StreamObserver<HelloRequest>() {
            @Override
            public void onNext(final HelloRequest request) {
                HelloResponse responseData = HelloResponse.newBuilder()
                        .setData("hello: " + request.getData())
                        .build();
                responseObserver.onNext(responseData);
            }

            @Override
            public void onError(final Throwable t) {
                LOG.error(t.getMessage());
            }

            @Override
            public void onCompleted() {
                HelloResponse responseData = HelloResponse.newBuilder()
                        .setData("hello onCompleted")
                        .build();
                responseObserver.onNext(responseData);
                responseObserver.onCompleted();
            }
        };
    }
}
