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

package org.apache.shenyu.examples.sdk.grpc.consumer.controller;

import org.apache.shenyu.examples.sdk.grpc.consumer.api.ShenyuGrpcClientApi;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.EchoRequest;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.EchoResponse;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.GrpcData;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.HelloRequest;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.HelloResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;
import java.util.List;

@RestController
public class ShenyuSdkGrpcConsumerController {

    @Autowired
    private ShenyuGrpcClientApi shenyuGrpcClientApi;

    /**
     * hello.
     *
     * @param request request
     * @return HelloResponse
     */
    @PostMapping("/hello")
    public List<HelloResponse> hello(final @RequestBody HelloRequest request) {
        return shenyuGrpcClientApi.hello(new GrpcData<HelloRequest>(Collections.singletonList(request)));
    }

    /**
     * echo.
     *
     * @param echoRequest echoRequest
     * @return EchoResponse
     */
    @PostMapping("/echo")
    public List<EchoResponse> echo(final @RequestBody EchoRequest echoRequest) {
        return shenyuGrpcClientApi.echo(new GrpcData<>(Collections.singletonList(echoRequest)));
    }

}
