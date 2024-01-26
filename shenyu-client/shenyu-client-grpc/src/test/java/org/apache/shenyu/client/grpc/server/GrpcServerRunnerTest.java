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

import io.grpc.ServerBuilder;
import org.apache.shenyu.client.grpc.GrpcClientEventListener;
import org.junit.jupiter.api.Test;
import org.springframework.context.event.ContextRefreshedEvent;

import static org.mockito.Mockito.mock;

public class GrpcServerRunnerTest {
    private final ContextRefreshedEvent testEvent = mock(ContextRefreshedEvent.class);

    private final GrpcClientEventListener testGrpcClientEventListener = mock(GrpcClientEventListener.class);

    @Test
    public void testOnApplicationEvent() {
        GrpcServerBuilder testGrpcServerBuilder = () -> ServerBuilder.forPort(8088);

        GrpcServerRunner testGrpcServerRunner = new GrpcServerRunner(testGrpcServerBuilder, testGrpcClientEventListener);

        testGrpcServerRunner.onApplicationEvent(testEvent);
    }
}
