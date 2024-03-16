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

package org.apache.shenyu.plugin.metrics.constant;

/**
 * The Label names.
 */
public final class LabelNames {
    
    /**
     * The constant REQUEST_TOTAL.
     */
    public static final String REQUEST_TOTAL = "shenyu_request_total";
    
    /**
     * The constant REQUEST_TYPE_TOTAL.
     */
    public static final String REQUEST_TYPE_TOTAL = "shenyu_request_type_total";
    
    /**
     * The constant REQUEST_THROW_TOTAL.
     */
    public static final String REQUEST_THROW_TOTAL = "shenyu_request_throw_total";
    
    /**
     * The constant EXECUTE_LATENCY_NAME.
     */
    public static final String EXECUTE_LATENCY_NAME = "shenyu_execute_latency_millis";

    /**
     * The constant RATELIMITER_REQUEST_RESTRICT_TOTAL.
     */
    public static final String RATELIMITER_REQUEST_RESTRICT_TOTAL = "shenyu_ratelimiter_request_restrict_total";

    /**
     * The constant SENTINEL_REQUEST_RESTRICT_TOTAL.
     */
    public static final String SENTINEL_REQUEST_RESTRICT_TOTAL = "shenyu_sentinel_request_restrict_total";

    /**
     * The constant SENTINEL_REQUEST_CIRCUITBREAKER_TOTAL.
     */
    public static final String SENTINEL_REQUEST_CIRCUITBREAKER_TOTAL = "shenyu_sentinel_request_circuitbreaker_total";

    /**
     * The constant RESILIENCE4J_REQUEST_RESTRICT_TOTAL.
     */
    public static final String RESILIENCE4J_REQUEST_RESTRICT_TOTAL = "shenyu_resilience4j_request_restrict_total";

    /**
     * The constant RESILIENCE4J_REQUEST_CIRCUITBREAKER_TOTAL.
     */
    public static final String RESILIENCE4J_REQUEST_CIRCUITBREAKER_TOTAL = "shenyu_resilience4j_request_circuitbreaker_total";

    /**
     * The constant HYSTRIX_REQUEST_CIRCUITBREAKER_TOTAL.
     */
    public static final String HYSTRIX_REQUEST_CIRCUITBREAKER_TOTAL = "shenyu_hystrix_request_circuitbreaker_total";
}
