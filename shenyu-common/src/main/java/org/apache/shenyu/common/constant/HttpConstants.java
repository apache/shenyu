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

package org.apache.shenyu.common.constant;

import java.util.concurrent.TimeUnit;

/**
 * constants for http, including http long polling.
 *
 * @since 2.0.0
 */
public final class HttpConstants {

    /**
     * Client long polling timeout is 90s.
     */
    public static final long CLIENT_POLLING_READ_TIMEOUT = TimeUnit.SECONDS.toMillis(90);

    /**
     * The maximum timeout of server block is 60s.
     */
    public static final long SERVER_MAX_HOLD_TIMEOUT = TimeUnit.SECONDS.toMillis(60);

    /**
     * Default connection timeout is 10s.
     */
    public static final long CLIENT_POLLING_CONNECT_TIMEOUT = TimeUnit.SECONDS.toMillis(10);

    /**
     * Default write timeout is 90s.
     */
    public static final long CLIENT_POLLING_WRITE_TIMEOUT = TimeUnit.SECONDS.toMillis(90);
}
