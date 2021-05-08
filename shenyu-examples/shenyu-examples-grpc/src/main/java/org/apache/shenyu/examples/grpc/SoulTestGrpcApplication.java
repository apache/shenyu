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

package org.apache.shenyu.examples.grpc;

import io.grpc.ServerBuilder;
import io.grpc.protobuf.services.ProtoReflectionService;
import org.apache.shenyu.examples.grpc.echo.EchoServiceImpl;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.io.IOException;

/**
 * SoulTestTarsApplication.
 * @author tydhot
 */
@SpringBootApplication
public class SoulTestGrpcApplication {

    /**
     * main.
     *
     * @param args args
     */
    public static void main(final String[] args) throws IOException {
        SpringApplication.run(SoulTestGrpcApplication.class, args);
        io.grpc.Server server = ServerBuilder
                .forPort(8080)
                .addService(new EchoServiceImpl())
                .addService(ProtoReflectionService.newInstance())
                .build();
        server.start();
    }
}
