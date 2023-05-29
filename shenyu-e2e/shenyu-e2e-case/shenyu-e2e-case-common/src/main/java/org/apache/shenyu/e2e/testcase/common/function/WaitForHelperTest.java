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

import io.restassured.builder.ResponseSpecBuilder;
import io.restassured.http.Method;
import org.apache.shenyu.e2e.testcase.common.function.WaitForHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.concurrent.TimeoutException;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.containsString;

public class WaitForHelperTest {
    
    @Test
    void testSuccess() throws TimeoutException {
        new WaitForHelper().waitFor(
                () -> given().baseUri("http://httpbin.org").when(),
                Method.GET,
                "/delay/0",
                new ResponseSpecBuilder().expectBody("url", containsString("/delay/0")).build()
        );
    }
    
    @Test
    void testTimeout() {
        Assertions.assertThrows(TimeoutException.class, () -> {
            new WaitForHelper(10, Duration.ofSeconds(1), Duration.ofMillis(500)).waitFor(
                    () -> given().baseUri("http://httpbin.org").when(),
                    Method.GET,
                    "/delay/1x",
                    new ResponseSpecBuilder().expectBody("message", containsString("/delay/1x")).build()
            );
        });
    }
}
