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

import com.google.common.util.concurrent.MoreExecutors;
import io.restassured.http.Method;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;
import io.restassured.specification.ResponseSpecification;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.slf4j.MDC;

import java.time.Duration;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;

@Slf4j
@NoArgsConstructor
@AllArgsConstructor
public class WaitForHelper {
    private static final ExecutorService executor = MoreExecutors.getExitingExecutorService((ThreadPoolExecutor) Executors.newFixedThreadPool(1));
    
    public int retryTimes = 30;
    
    public Duration timeInRetry = Duration.ofSeconds(3);
    
    public Duration timeout = Duration.ofMinutes(3);
    
    public void waitFor(Supplier<RequestSpecification> supplier, Method method, String endpoint, ResponseSpecification expected) throws TimeoutException {
        final Map<String, String> contextMap = MDC.getCopyOfContextMap();
        Future<?> future = executor.submit(() -> {
            MDC.setContextMap(contextMap);
            
            for (int i = 0; i < retryTimes; i++) {
                try {
                    ValidatableResponse response = supplier.get()
                            .request(method, endpoint)
                            .then()
                            .assertThat();
                    response.spec(expected);
                    
                    if (log.isDebugEnabled()) {
                        log.debug(response.extract().asPrettyString());
                    }
                    return;
                } catch (AssertionError e) {
                    log.debug("failed to check endpoint '" + endpoint + "'\n {}", e.getMessage());
                }
                
                try {
                    TimeUnit.MILLISECONDS.sleep(timeInRetry.toMillis());
                } catch (InterruptedException ignore) {
                }
            }
        });
        
        try {
            future.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
            log.info("check endpoint({}) successful", endpoint);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            future.cancel(true);
            throw new TimeoutException("checking endpoint '" + endpoint + "' timeout after " + timeout);
        }
    }
    
    
    public void waitFor(Supplier<RequestSpecification> supplier, HttpChecker checker) throws TimeoutException {
        final Map<String, String> contextMap = MDC.getCopyOfContextMap();
        Future<?> future = executor.submit(() -> {
            MDC.setContextMap(contextMap);
            
            for (int i = 0; i < retryTimes; i++) {
                try {
                    checker.check(supplier);
                    return;
                } catch (AssertionError e) {
                    log.debug(e.getMessage());
                }
                
                try {
                    TimeUnit.MILLISECONDS.sleep(timeInRetry.toMillis());
                } catch (InterruptedException ignore) {
                    break;
                }
            }
            log.info("check endpoint({}) successful", MDC.get("endpoint"));
        });
        
        try {
            future.get(timeout.toMillis(), TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            future.cancel(true);
            throw new TimeoutException("checking endpoint '" + MDC.get("endpoint") + "' timeout after " + timeout);
        }
    }
    
    public static void waitForEffecting(Supplier<RequestSpecification> supplier, Method method, String endpoint, ResponseSpecification expected) {
        Assertions.assertDoesNotThrow(() -> new WaitForHelper().waitFor(supplier, method, endpoint, expected), "waiting for endpoint to take effect");
    }
    
    public static void waitForEffecting(Supplier<RequestSpecification> supplier, String endpoint, ResponseSpecification expected) {
        Assertions.assertDoesNotThrow(() -> new WaitForHelper().waitFor(supplier, Method.GET, endpoint, expected), "waiting for endpoint to take effect");
    }
    
    public static void waitForEffecting(Supplier<RequestSpecification> supplier, HttpChecker checker) {
        Assertions.assertDoesNotThrow(() -> new WaitForHelper().waitFor(supplier, checker), "waiting for endpoint to take effect");
    }
}
