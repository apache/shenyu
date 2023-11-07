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

package org.apache.shenyu.e2e.testcase.grpc.sync;

import com.google.common.collect.Lists;
import org.apache.shenyu.e2e.engine.scenario.ShenYuScenarioProvider;
import org.apache.shenyu.e2e.engine.scenario.specification.ScenarioSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuBeforeEachSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuCaseSpec;
import org.apache.shenyu.e2e.engine.scenario.specification.ShenYuScenarioSpec;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import io.restassured.http.Method;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;

public class GrpcPluginCases implements ShenYuScenarioProvider {

    @Override
    public List<ScenarioSpec> get() {
        return Lists.newArrayList(
                testGrpc()
        );
    }

    private ShenYuScenarioSpec testGrpc() {
        Map<String, List<MessageData>> body = new ConcurrentHashMap<>();
        body.put("data", Lists.newArrayList(new MessageData("hello grpc")));
        return ShenYuScenarioSpec.builder()
                .name("test grpc invoker")
                .beforeEachSpec(
                        ShenYuBeforeEachSpec.builder()
                                .checker(notExists("/sofa/findAll"))
                                .checker(exists(Method.POST, "/grpc/echo", body))
                                .build()
                )
                .caseSpec(
                        ShenYuCaseSpec.builder()
                                .addExists(Method.POST, "/grpc/echo", body)
                                .addNotExists("/grpc/fin")
                                .addNotExists("/put")
                                .addNotExists("/get")
                                .build())
                .build();
    }
    
    public static class MessageData {

        /**
         * message.
         */
        private String message;

        /**
         * default constructor.
         */
        public MessageData() {
        }

        public MessageData(final String message) {
            this.message = message;
        }

        /**
         * get message.
         *
         * @return message
         */
        public String getMessage() {
            return message;
        }

        /**
         * set message.
         *
         * @param message message
         */
        public void setMessage(final String message) {
            this.message = message;
        }

    }
}
