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

package org.apache.shenyu.e2e.testcase.common.function;

import io.restassured.http.Method;
import org.junit.jupiter.api.Assertions;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

public class HttpCheckers {
    
    public static HttpChecker notExists(String endpoint) {
        return notExists(Method.GET, endpoint);
    }
    
    public static HttpChecker notExists(Method method, String endpoint) {
        return (request) -> {
            try {
                request.request(method, endpoint)
                        .then()
                        .log()
                        .ifValidationFails()
                        .body("code", lessThan(0))
                        .body("message", containsString("please check your configuration!"));
            } catch (AssertionError error) {
                Assertions.fail("endpoint '" + endpoint + "' already exists, but expected it does not exist.", error);
            }
        };
    }
    
    public static HttpChecker exists(String endpoint) {
        return exists(Method.GET, endpoint);
    }
    
    public static HttpChecker exists(Method method, String endpoint) {
        return (request) -> {
            try {
                request.request(method, endpoint)
                        .then()
                        .log()
                        .ifValidationFails()
                        .body("code", nullValue())
                        .body("message", not(containsString("please check your configuration!")));
            } catch (AssertionError error) {
                Assertions.fail("endpoint '" + endpoint + "' not exists", error);
            }
        };
    }
    
}
