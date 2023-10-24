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

package org.apache.shenyu.e2e.engine.function;

import org.apache.shenyu.e2e.engine.scenario.function.WaitForHelper;
import io.restassured.builder.ResponseSpecBuilder;
import io.restassured.http.Method;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.concurrent.TimeoutException;

import static io.restassured.RestAssured.given;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;

public class WaitForHelperTest {
    
    @Test
    void testSuccess() throws TimeoutException {
        new WaitForHelper().waitFor(
                () -> given().baseUri("https://postman-echo.com").when(),
                Method.GET,
                "/get?foo1=bar1&foo2=bar2",
                new ResponseSpecBuilder().expectBody("url", containsString("/get")).build()
        );
    }
    
    @Test
    void testTimeout() {
        Assertions.assertThrows(TimeoutException.class, () ->
            new WaitForHelper(10, Duration.ofSeconds(1), Duration.ofMillis(500)).waitFor(
                    () -> given().baseUri("https://postman-echo.com").when(),
                    Method.GET,
                    "/delay/2",
                    new ResponseSpecBuilder().expectBody("delay", equalTo(2)).build()
            )
        );
    }
}
