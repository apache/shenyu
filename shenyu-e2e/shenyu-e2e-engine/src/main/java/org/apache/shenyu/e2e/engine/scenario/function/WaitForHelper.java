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

package org.apache.shenyu.e2e.engine.scenario.function;

import com.google.common.util.concurrent.MoreExecutors;
import io.restassured.http.Method;
import io.restassured.response.ValidatableResponse;
import io.restassured.specification.RequestSpecification;
import io.restassured.specification.ResponseSpecification;
import org.junit.jupiter.api.Assertions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

/**
 * Wait for checking endpoint.
 */
public class WaitForHelper {

    private static final Logger log = LoggerFactory.getLogger(WaitForHelper.class);

    private static final ExecutorService EXECUTOR = MoreExecutors.getExitingExecutorService((ThreadPoolExecutor) Executors.newFixedThreadPool(1));
    
    private int retryTimes = 30;
    
    private Duration timeInRetry = Duration.ofSeconds(3);
    
    private Duration timeout = Duration.ofMinutes(3);

    public WaitForHelper() {
    }

    public WaitForHelper(final int retryTimes, final Duration timeInRetry, final Duration timeout) {
        this.retryTimes = retryTimes;
        this.timeInRetry = timeInRetry;
        this.timeout = timeout;
    }

    /**
     * Check if the endpoint is successful. If unsuccessful, retry until the maximum number of times is reached.
     *
     * @param supplier supplier
     * @param method method
     * @param endpoint endpoint
     * @param expected expected
     * @throws TimeoutException TimeoutException
     */
    public void waitFor(final Supplier<RequestSpecification> supplier, final Method method, final String endpoint, final ResponseSpecification expected) throws TimeoutException {
        final Map<String, String> contextMap = MDC.getCopyOfContextMap();
        Future<?> future = EXECUTOR.submit(() -> {
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
                    log.debug("failed to check endpoint '{}'\n {}", endpoint, e.getMessage());
                }
                
                try {
                    TimeUnit.MILLISECONDS.sleep(timeInRetry.toMillis());
                } catch (InterruptedException ex) {
                    log.error(ex.getMessage(), ex);
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

    /**
     * Check if the endpoint is successful. If unsuccessful, retry until the maximum number of times is reached.
     *
     * @param supplier supplier
     * @param checker checker
     * @throws TimeoutException TimeoutException
     */
    public void waitFor(final Supplier<RequestSpecification> supplier, final HttpChecker checker) throws TimeoutException {
        final Map<String, String> contextMap = MDC.getCopyOfContextMap();
        Future<?> future = EXECUTOR.submit(() -> {
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
                } catch (InterruptedException ex) {
                    log.error(ex.getMessage(), ex);
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
    
    /**
     * wait for effecting.
     * @param supplier supplier
     * @param method method
     * @param endpoint endpoint
     * @param expected expected
     */
    public static void waitForEffecting(final Supplier<RequestSpecification> supplier, final Method method, final String endpoint, final ResponseSpecification expected) {
        Assertions.assertDoesNotThrow(() -> new WaitForHelper().waitFor(supplier, method, endpoint, expected), "waiting for endpoint to take effect");
    }
    
    /**
     * wait for effecting.
     * @param supplier supplier
     * @param endpoint endpoint
     * @param expected expected
     */
    public static void waitForEffecting(final Supplier<RequestSpecification> supplier, final String endpoint, final ResponseSpecification expected) {
        Assertions.assertDoesNotThrow(() -> new WaitForHelper().waitFor(supplier, Method.GET, endpoint, expected), "waiting for endpoint to take effect");
    }
    
    /**
     * wait for effecting.
     * @param supplier supplier
     * @param checker checker
     */
    public static void waitForEffecting(final Supplier<RequestSpecification> supplier, final HttpChecker checker) {
        Assertions.assertDoesNotThrow(() -> new WaitForHelper().waitFor(supplier, checker), "waiting for endpoint to take effect");
    }
}
