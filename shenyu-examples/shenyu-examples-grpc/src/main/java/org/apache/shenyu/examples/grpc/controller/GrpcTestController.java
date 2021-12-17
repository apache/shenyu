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

package org.apache.shenyu.examples.grpc.controller;

import echo.EchoRequest;
import echo.EchoResponse;
import echo.EchoServiceGrpc;
import io.grpc.Channel;
import io.grpc.ManagedChannelBuilder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * GrpcTestController.
 */
@RestController
@RequestMapping("/test/grpc")
public class GrpcTestController {

    private final Channel channel = channel();

    /**
     * test grpc.
     *
     * @return hello world
     */
    @GetMapping("/hello")
    public String hello() {
        EchoServiceGrpc.EchoServiceBlockingStub stub = EchoServiceGrpc.newBlockingStub(channel);
        EchoRequest request = EchoRequest.newBuilder().setMessage("hello").build();
        EchoResponse response = stub.echo(request);
        return response.getMessage();
    }

    private Channel channel() {
        return ManagedChannelBuilder.forAddress("127.0.0.1", 8080)
                .usePlaintext().build();
    }

}
