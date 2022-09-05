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

package org.apache.shenyu.plugin.logging.common.constant;

/**
 * generic logging constant.
 */
public class GenericLoggingConstant {
    
    /**
     * logging user agent.
     */
    public static final String USER_AGENT = "User-Agent";
    
    /**
     * logging user host.
     */
    public static final String HOST = "Host";
    
    /**
     * shenyu agent trace id.
     */
    public static final String SHENYU_AGENT_TRACE_ID = "shenyu-agent-trace-id";
    
    /**
     * system default, max threads.
     */
    public static final Integer MAX_ALLOW_THREADS = 500;
    
    /**
     * max queue.
     */
    public static final Integer MAX_QUEUE_NUMBER = 10000;
    
    /**
     * default source.
     */
    public static final String DEFAULT_SOURCE = "shenyu-gateway";
    
    /**
     * The constant INDEX.
     */
    public static final String INDEX = "shenyu-access-logging";

}
