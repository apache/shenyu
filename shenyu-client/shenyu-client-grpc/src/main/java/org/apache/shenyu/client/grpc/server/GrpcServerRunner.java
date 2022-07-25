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

package org.apache.shenyu.client.grpc.server;

import io.grpc.Server;
import io.grpc.ServerBuilder;
import io.grpc.ServerServiceDefinition;
import org.apache.shenyu.client.grpc.GrpcClientEventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;

import java.io.IOException;
import java.util.List;

/**
 * Add grpc service and start grpc server.
 */
public class GrpcServerRunner implements ApplicationRunner {

    private static final Logger LOG = LoggerFactory.getLogger(GrpcServerRunner.class);

    private final GrpcServerBuilder grpcServerBuilder;

    private final GrpcClientEventListener grpcClientEventListener;

    public GrpcServerRunner(final GrpcServerBuilder grpcServerBuilder,
                            final GrpcClientEventListener grpcClientEventListener) {
        this.grpcServerBuilder = grpcServerBuilder;
        this.grpcClientEventListener = grpcClientEventListener;
    }

    @Override
    public void run(final ApplicationArguments args) {
        startGrpcServer();
    }

    private void startGrpcServer() {
        ServerBuilder<?> serverBuilder = grpcServerBuilder.buildServerBuilder();

        List<ServerServiceDefinition> serviceDefinitions = grpcClientEventListener.getServiceDefinitions();
        for (ServerServiceDefinition serviceDefinition : serviceDefinitions) {
            serverBuilder.addService(serviceDefinition);
            LOG.info("{} has been add to grpc server", serviceDefinition.getServiceDescriptor().getName());
        }

        try {
            Server server = serverBuilder.build().start();

            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                LOG.info("shutting down grpc server");
                server.shutdown();
                LOG.info("grpc server shut down");
            }));

            LOG.info("Grpc server started successfully");
        } catch (IOException e) {
            LOG.error("Grpc server failed to start", e);
        }
    }
}
