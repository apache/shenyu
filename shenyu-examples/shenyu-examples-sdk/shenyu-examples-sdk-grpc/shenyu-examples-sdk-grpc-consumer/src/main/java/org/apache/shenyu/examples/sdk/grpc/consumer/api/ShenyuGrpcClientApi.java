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

package org.apache.shenyu.examples.sdk.grpc.consumer.api;

import org.apache.shenyu.examples.sdk.grpc.consumer.dto.EchoRequest;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.EchoResponse;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.GrpcData;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.HelloRequest;
import org.apache.shenyu.examples.sdk.grpc.consumer.dto.HelloResponse;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@ShenyuClient(value = "shenyu-gateway")
public interface ShenyuGrpcClientApi {

    /**
     * hello.
     *
     * @param request request
     * @return response
     */
    @PostMapping("/grpc/helloService/hello")
    List<HelloResponse> hello(@RequestBody GrpcData<HelloRequest> request);

    /**
     * echo.
     *
     * @param echoRequest echoRequest
     * @return echoResponse
     */
    @PostMapping("/grpc/echo")
    List<EchoResponse> echo(@RequestBody GrpcData<EchoRequest> echoRequest);
}
