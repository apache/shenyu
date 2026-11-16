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

package org.apache.shenyu.admin.service.fencing;

import java.util.Objects;

/**
 * Thread-bound marker for a validated synchronous fenced write.
 */
public final class RegistrationFencingContext implements AutoCloseable {

    private static final ThreadLocal<RegistrationFencingContext> CURRENT = new ThreadLocal<>();

    private final String targetId;

    private final String selectorId;

    private final String sessionId;

    private final long generation;

    private RegistrationFencingContext(final String targetId, final String selectorId,
                                       final String sessionId, final long generation) {
        this.targetId = targetId;
        this.selectorId = selectorId;
        this.sessionId = sessionId;
        this.generation = generation;
    }

    /**
     * Enter a validated write context.
     *
     * @param targetId target id
     * @param selectorId selector id
     * @param sessionId session id
     * @param generation generation
     * @return closeable context
     */
    public static RegistrationFencingContext enter(final String targetId, final String selectorId,
                                                   final String sessionId, final long generation) {
        RegistrationFencingContext context = new RegistrationFencingContext(targetId, selectorId, sessionId, generation);
        CURRENT.set(context);
        return context;
    }

    /**
     * Check whether the current transaction is authorized for a target.
     *
     * @param targetId target id
     * @param selectorId selector id
     * @return whether the context matches
     */
    public static boolean matches(final String targetId, final String selectorId) {
        RegistrationFencingContext context = CURRENT.get();
        return Objects.nonNull(context)
                && Objects.equals(context.targetId, targetId)
                && Objects.equals(context.selectorId, selectorId);
    }

    /**
     * Check whether the current transaction authorizes an exact target.
     *
     * @param targetId target id
     * @param selectorId selector id
     * @param generation generation
     * @return whether the context matches
     */
    public static boolean matches(final String targetId, final String selectorId, final long generation) {
        RegistrationFencingContext context = CURRENT.get();
        return Objects.nonNull(context)
                && Objects.equals(context.targetId, targetId)
                && Objects.equals(context.selectorId, selectorId)
                && context.generation == generation;
    }

    /**
     * Check whether the current transaction belongs to a selector.
     *
     * @param selectorId selector id
     * @return whether the context matches
     */
    public static boolean matchesSelector(final String selectorId) {
        RegistrationFencingContext context = CURRENT.get();
        return Objects.nonNull(context) && Objects.equals(context.selectorId, selectorId);
    }

    /**
     * Get current context.
     *
     * @return current context or null
     */
    public static RegistrationFencingContext current() {
        return CURRENT.get();
    }

    /**
     * Get session id.
     *
     * @return session id
     */
    public String getSessionId() {
        return sessionId;
    }

    /**
     * Get generation.
     *
     * @return generation
     */
    public long getGeneration() {
        return generation;
    }

    @Override
    public void close() {
        CURRENT.remove();
    }
}
