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

package org.apache.shenyu.agent.plugin.tracing.common.constant;

/**
 * The type Tracing constants.
 */
public final class TracingConstants {

    /**
     * The constant NAME.
     */
    public static final String NAME = "shenyu";

    /**
     * The constant ROOT_SPAN.
     */
    public static final String ROOT_SPAN = "/shenyu/root";

    public static final String COMPONENT = "component";

    public static final String HTTP_URL = "http.url";

    public static final String HTTP_STATUS = "http.status_code";

    public static final String SHENYU_AGENT = "shenyu-agent";

    /**
     * The type Error log tags.
     */
    public static final class ErrorLogTags {

        /**
         * The constant EVENT.
         */
        public static final String EVENT = "event";

        /**
         * The constant EVENT_ERROR_TYPE.
         */
        public static final String EVENT_ERROR_TYPE = "error";

        /**
         * The constant ERROR_KIND.
         */
        public static final String ERROR_KIND = "error.kind";

        /**
         * The constant MESSAGE.
         */
        public static final String MESSAGE = "message";
    }
}
