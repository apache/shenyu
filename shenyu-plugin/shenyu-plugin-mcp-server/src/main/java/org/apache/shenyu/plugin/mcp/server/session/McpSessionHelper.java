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

package org.apache.shenyu.plugin.mcp.server.session;

import io.modelcontextprotocol.server.McpAsyncServerExchange;
import io.modelcontextprotocol.server.McpSyncServerExchange;
import io.modelcontextprotocol.spec.McpServerSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.model.ToolContext;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;

/**
 * Helper class for handling McpSession related operations.
 *
 * <p>SDK Compatibility Note: This class uses reflection to access internal SDK fields
 * that may change between versions. The following SDK versions are tested and supported:
 * <ul>
 *   <li>MCP SDK 0.17.0 (current)</li>
 *   <li>Spring AI 1.1.2 (current)</li>
 * </ul>
 *
 * <p>If reflection fails (e.g., due to SDK API changes), this class will throw
 * IllegalStateException with clear error messages indicating SDK compatibility issues.
 *
 * @since 2.7.0.2
 */
public class McpSessionHelper {

    private static final Logger LOG = LoggerFactory.getLogger(McpSessionHelper.class);

    /**
     * SDK version for compatibility checking.
     * This should be updated when testing with new SDK versions.
     */
    private static final String SUPPORTED_SDK_VERSION = "0.17.0";

    /**
     * Cached field reference for McpSyncServerExchange.exchange field.
     * Null if not yet resolved or resolution failed.
     */
    private static volatile Field asyncExchangeFieldCache;

    /**
     * Cached field reference for McpAsyncServerExchange.session field.
     * Null if not yet resolved or resolution failed.
     */
    private static volatile Field sessionFieldCache;

    /**
     * Flag indicating whether reflection fields have been resolved.
     */
    private static volatile boolean fieldsResolved;

    /**
     * Lock for resolving reflection fields.
     */
    private static final Object FIELD_RESOLVE_LOCK = new Object();

    static {
        resolveReflectionFields();
    }

    /**
     * Resolves reflection fields for accessing internal SDK state.
     * This method is called during class initialization and logs any compatibility issues.
     */
    private static void resolveReflectionFields() {
        try {
            // Resolve asyncExchange field from McpSyncServerExchange
            asyncExchangeFieldCache = McpSyncServerExchange.class.getDeclaredField("exchange");
            asyncExchangeFieldCache.setAccessible(true);
            LOG.info("Successfully resolved McpSyncServerExchange.exchange field for SDK compatibility");

            // Resolve session field from McpAsyncServerExchange
            sessionFieldCache = McpAsyncServerExchange.class.getDeclaredField("session");
            sessionFieldCache.setAccessible(true);
            LOG.info("Successfully resolved McpAsyncServerExchange.session field for SDK compatibility");

            fieldsResolved = true;
            LOG.info("MCP SDK reflection fields resolved successfully. Tested with SDK version: {}", SUPPORTED_SDK_VERSION);
        } catch (NoSuchFieldException e) {
            LOG.error("SDK COMPATIBILITY ERROR: Failed to resolve reflection fields. "
                    + "This indicates the MCP SDK API has changed. "
                    + "Tested version: {}, Current SDK may be incompatible. "
                    + "Missing field: {}", SUPPORTED_SDK_VERSION, e.getMessage());
            fieldsResolved = false;
        } catch (SecurityException e) {
            LOG.error("SDK COMPATIBILITY ERROR: Security manager blocked reflection access. "
                    + "Field resolution failed: {}", e.getMessage());
            fieldsResolved = false;
        }
    }

    /**
     * Get McpSyncServerExchange from ToolContext.
     *
     * @param toolContext the tool context
     * @return the McpSyncServerExchange instance
     */
    public static McpSyncServerExchange getMcpSyncServerExchange(final ToolContext toolContext) {
        if (Objects.isNull(toolContext)) {
            throw new IllegalArgumentException("ToolContext is required");
        }
        Map<String, Object> contextMap = toolContext.getContext();
        if (Objects.isNull(contextMap) || contextMap.isEmpty()) {
            throw new IllegalArgumentException("ToolContext is required");
        }
        McpSyncServerExchange mcpSyncServerExchange = (McpSyncServerExchange) contextMap.get("exchange");
        if (Objects.isNull(mcpSyncServerExchange)) {
            throw new IllegalArgumentException("McpSyncServerExchange is required in ToolContext");
        }
        return mcpSyncServerExchange;
    }

    /**
     * Get sessionId from McpSyncServerExchange.
     *
     * <p>Uses reflection to access internal SDK fields. If reflection fails,
     * an IllegalStateException is thrown with SDK compatibility information.
     *
     * @param mcpSyncServerExchange the McpSyncServerExchange instance
     * @return the session id string
     * @throws IllegalStateException if SDK reflection fails (API incompatibility)
     */
    public static String getSessionId(final McpSyncServerExchange mcpSyncServerExchange) {
        McpServerSession session = getSession(mcpSyncServerExchange);
        if (Objects.isNull(session)) {
            throw new IllegalArgumentException("Session is required in McpAsyncServerExchange");
        }
        return session.getId();
    }

    /**
     * Get McpServerSession from McpSyncServerExchange.
     *
     * <p>Uses reflection to access internal SDK fields. If reflection fails,
     * an IllegalStateException is thrown with SDK compatibility information.
     *
     * @param mcpSyncServerExchange the McpSyncServerExchange instance
     * @return the McpServerSession instance
     * @throws IllegalStateException if SDK reflection fails (API incompatibility)
     */
    public static McpServerSession getSession(final McpSyncServerExchange mcpSyncServerExchange) {
        checkReflectionAvailability();

        try {
            Object asyncExchange = asyncExchangeFieldCache.get(mcpSyncServerExchange);
            if (Objects.isNull(asyncExchange)) {
                throw new IllegalArgumentException("McpAsyncServerExchange is required in McpSyncServerExchange");
            }
            McpAsyncServerExchange mcpAsyncServerExchange = (McpAsyncServerExchange) asyncExchange;
            Object session = sessionFieldCache.get(mcpAsyncServerExchange);
            if (Objects.isNull(session)) {
                throw new IllegalArgumentException("Session is required in McpAsyncServerExchange");
            }
            return (McpServerSession) session;
        } catch (IllegalAccessException e) {
            throw new IllegalStateException(
                    "SDK COMPATIBILITY ERROR: Failed to access SDK internal fields via reflection. "
                    + "This indicates the MCP SDK API has changed. "
                    + "Tested SDK version: " + SUPPORTED_SDK_VERSION + ". "
                    + "Error: " + e.getMessage(), e);
        }
    }

    /**
     * Checks if reflection fields are available and throws an informative exception if not.
     *
     * @throws IllegalStateException if reflection fields are not available
     */
    private static void checkReflectionAvailability() {
        if (!fieldsResolved || Objects.isNull(asyncExchangeFieldCache) || Objects.isNull(sessionFieldCache)) {
            // Attempt to re-resolve fields in case of delayed class loading
            synchronized (FIELD_RESOLVE_LOCK) {
                if (!fieldsResolved) {
                    resolveReflectionFields();
                }
            }

            if (!fieldsResolved || Objects.isNull(asyncExchangeFieldCache) || Objects.isNull(sessionFieldCache)) {
                throw new IllegalStateException(
                        "SDK COMPATIBILITY ERROR: MCP SDK reflection fields are not available. "
                        + "The MCP SDK version may be incompatible with this implementation. "
                        + "Tested SDK version: " + SUPPORTED_SDK_VERSION + ". "
                        + "Please verify SDK version compatibility or check logs for field resolution errors.");
            }
        }
    }

    /**
     * Checks if the SDK reflection fields are available for use.
     * This can be used for proactive compatibility checking.
     *
     * @return true if reflection fields are resolved and available
     */
    public static boolean isReflectionAvailable() {
        return fieldsResolved && Objects.nonNull(asyncExchangeFieldCache) && Objects.nonNull(sessionFieldCache);
    }

    /**
     * Returns the SDK version that this implementation has been tested with.
     *
     * @return the supported SDK version string
     */
    public static String getSupportedSdkVersion() {
        return SUPPORTED_SDK_VERSION;
    }
}
