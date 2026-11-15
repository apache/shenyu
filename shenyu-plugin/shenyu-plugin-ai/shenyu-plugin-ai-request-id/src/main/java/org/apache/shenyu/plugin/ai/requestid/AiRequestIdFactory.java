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

package org.apache.shenyu.plugin.ai.requestid;

import org.apache.commons.lang3.StringUtils;

import java.util.UUID;
import java.util.regex.Pattern;

/**
 * Generates and validates bounded AI request identifiers.
 */
public final class AiRequestIdFactory {

    private static final int MAX_LENGTH = 128;

    private static final Pattern REUSABLE = Pattern.compile("[A-Za-z0-9._:-]{1," + MAX_LENGTH + "}");

    private AiRequestIdFactory() {
    }

    /**
     * Generate a gateway-owned opaque request id.
     *
     * @return generated request id
     */
    public static String generate() {
        return UUID.randomUUID().toString();
    }

    /**
     * Whether an existing context identifier can be reused without rewriting it.
     * The bounded character set keeps the id safe for response headers and access logs.
     *
     * @param requestId existing request id
     * @return true when the identifier is present and bounded
     */
    public static boolean reusable(final String requestId) {
        return StringUtils.isNotEmpty(requestId) && REUSABLE.matcher(requestId).matches();
    }
}
