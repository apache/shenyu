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

package org.apache.shenyu.e2e.engine.scenario.specification;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import io.restassured.http.Method;
import org.apache.shenyu.e2e.engine.scenario.function.Verifier;
import org.apache.shenyu.e2e.engine.scenario.function.WebSocketCheckers;
import org.apache.shenyu.e2e.engine.scenario.function.WebSocketVerifier;
import org.hamcrest.Matcher;

import java.util.List;
import java.util.Map;

import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.exists;
import static org.apache.shenyu.e2e.engine.scenario.function.HttpCheckers.notExists;

/**
 * ShenYu case specification.
 */
public class ShenYuCaseSpec implements CaseSpec {
    
    private final String name;
    
    private final List<Verifier> verifiers;

    private final List<WebSocketVerifier> webSocketVerifiers;

    public ShenYuCaseSpec(final String name, final List<Verifier> verifiers, final List<WebSocketVerifier> webSocketVerifiers) {
        this.name = name;
        this.verifiers = verifiers;
        this.webSocketVerifiers = webSocketVerifiers;
    }

    /**
     * get name.
     *
     * @return name
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * get verifiers.
     *
     * @return verifiers
     */
    @Override
    public List<Verifier> getVerifiers() {
        return verifiers;
    }

    @Override
    public List<WebSocketVerifier> getWebSocketVerifiers() {
        return webSocketVerifiers;
    }
    
    /**
     * builder.
     * @return ShenYuTestCaseSpecBuilder
     */
    public static ShenYuTestCaseSpecBuilder builder() {
        return new ShenYuTestCaseSpecBuilder();
    }
    
    /**
     * builder.
     * @param name name
     * @return ShenYuTestCaseSpecBuilder
     */
    public static ShenYuTestCaseSpecBuilder builder(final String name) {
        return new ShenYuTestCaseSpecBuilder(name);
    }

    public static class ShenYuTestCaseSpecBuilder {

        private String name;

        private final Builder<Verifier> builder = ImmutableList.builder();

        private final Builder<WebSocketVerifier> webSocketBuilder = ImmutableList.builder();

        public ShenYuTestCaseSpecBuilder() {
        }

        public ShenYuTestCaseSpecBuilder(final String name) {
            this.name = name;
        }
        
        /**
         * builder set name.
         * @param name name
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder name(final String name) {
            this.name = name;
            return this;
        }
        
        /**
         * builder add verifier.
         * @param verifier verifier
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder add(final Verifier verifier) {
            builder.add(verifier);
            return this;
        }

        /**
         * websocket builder add verifier.
         * @param webSocketVerifier webSocketVerifier
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder add(final WebSocketVerifier webSocketVerifier) {
            webSocketBuilder.add(webSocketVerifier);
            return this;
        }
        
        /**
         * add verifier case spec.
         * @param endpoint endpoint
         * @param matcher matcher
         * @param matchers matchers
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addVerifier(final String endpoint, final Matcher<?> matcher, final Matcher<?>... matchers) {
            return addVerifier(Method.GET, endpoint, matcher, matchers);
        }
        
        /**
         * add verifier case spec.
         * @param method method
         * @param endpoint endpoint
         * @param matcher matcher
         * @param matchers matchers
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addVerifier(final Method method, final String endpoint, final Matcher<?> matcher, final Matcher<?>... matchers) {
            return add((Verifier) supplier -> supplier.when().request(method, endpoint).then().assertThat().body(matcher, matchers));
        }
        
        /**
         * add exist endpoint case spec.
         * @param endpoint endpoint
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addExists(final String endpoint) {
            return addExists(Method.GET, endpoint);
        }
        
        /**
         * add exist method endpoint case spec.
         * @param method method
         * @param endpoint endpoint
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addExists(final Method method, final String endpoint) {
            return add(exists(method, endpoint));
        }

        /**
         * add exist method endpoint case spec.
         * @param endpoint endpoint
         * @param sendMessage sendMessage
         * @param receiveMessage receiveMessage
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addExists(final String endpoint, final String sendMessage, final String receiveMessage) {
            return add(WebSocketCheckers.exists(endpoint, sendMessage, receiveMessage));
        }


        /**
         * add exist method endpoint case spec.
         * @param method method
         * @param endpoint endpoint
         * @param body body
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addExists(final Method method, final String endpoint, final Map<String, ?> body) {
            return add(exists(method, endpoint, body));
        }


        /**
         * add not exist endpoint case spec.
         * @param endpoint endpoint
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addNotExists(final String endpoint) {
            return addNotExists(Method.GET, endpoint);
        }
        
        /**
         * add not exists case spec.
         * @param method method
         * @param endpoint endpoint
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addNotExists(final Method method, final String endpoint) {
            return add(notExists(method, endpoint));
        }

        /**
         * add not exists case spec.
         * @param endpoint endpoint
         * @param message message
         * @return ShenYuTestCaseSpecBuilder
         */
        public ShenYuTestCaseSpecBuilder addNotExists(final String endpoint, final String message) {
            return add(WebSocketCheckers.notExists(endpoint, message));
        }

        /**
         * build.
         * @return ShenYuCaseSpec
         */
        public ShenYuCaseSpec build() {
            return new ShenYuCaseSpec(name, builder.build(), webSocketBuilder.build());
        }
    }
}
