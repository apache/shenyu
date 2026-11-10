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

package org.apache.shenyu.common.ai;

import java.util.List;

/**
 * Bounded, immutable structural interpretation of one AI request body version.
 *
 * <p>The view is the shared governance metadata contract for the AI plugin chain: it
 * lets prompt, budget and proxy stages agree on <em>which</em> request interpretation
 * they acted on without each stage re-parsing the full body. It is intentionally
 * bounded — message entries and text prefixes are capped, structured content is never
 * flattened into text, and the original wire body is never retained here — so retained
 * memory does not grow with the request size. This type carries request content and
 * must therefore never be placed in {@code AgentRequestContext}, trace or audit
 * events, or any global map; it lives and dies with the server exchange.</p>
 *
 * <p>Two view kinds exist. The {@code WIRE} view interprets the original client body;
 * an {@code EFFECTIVE} view interprets a mutated body (for example a prompt plugin
 * decoration) and names the mutator. Every publication bumps a monotonically
 * increasing generation, so consumers can state which generation they inspected and
 * a stale interpretation is observable instead of silent.</p>
 *
 * @param protocol the protocol identity resolved for the request; {@code null} when
 *                 the identity stayed unresolved, in which case the body shape is
 *                 not reinterpreted as one
 * @param operation structural operation hint, e.g. {@code CHAT_COMPLETION};
 *                   {@code null} when unknown
 * @param model candidate model name from the body, or {@code null} when absent;
 *              the final model selection still belongs to the routing stage
 * @param stream streaming flag declared by the body, or the configured default
 * @param messages bounded message overview; never the full body
 * @param toolsPresent whether the body declares a non-empty tools array
 * @param toolCallsPresent whether any message carries tool calls
 * @param viewKind wire (original) or effective (post-mutation) view
 * @param mutator component that produced an effective view, {@code null} for wire views
 * @param generation monotonically increasing publication generation, one-based
 * @param unsupported true when the body could not be interpreted as a known AI protocol;
 *                    unsupported inspection must stay visible, never silently empty
 * @param unsupportedReason reason when unsupported, otherwise {@code null}
 */
public record AiNormalizedRequestView(String protocol, String operation, String model, boolean stream,
                                      List<BoundedMessage> messages, boolean toolsPresent, boolean toolCallsPresent,
                                      ViewKind viewKind, String mutator, long generation,
                                      boolean unsupported, String unsupportedReason) {

    /**
     * Maximum number of message entries kept in the bounded overview; further entries
     * are counted, not stored.
     */
    public static final int MAX_MESSAGES = 32;

    /**
     * Maximum number of characters kept of one textual message content; longer text is
     * marked truncated and only the prefix is retained.
     */
    public static final int MAX_MESSAGE_CHARS = 512;

    /**
     * Maximum number of entries the consumer inspection log retains; further distinct
     * consumers are not recorded.
     */
    public static final int MAX_INSPECTIONS = 8;

    public AiNormalizedRequestView {
        messages = List.copyOf(messages);
    }

    /**
     * Whether this view can be used for protocol-aware governance decisions.
     *
     * @return true when the body was interpreted as a known AI protocol
     */
    public boolean supported() {
        return !unsupported;
    }

    /**
     * How one message entry of the request is represented in the bounded overview.
     *
     * @param role declared role, e.g. {@code user} or {@code assistant}, or {@code null}
     * @param contentKind textual, structured (never flattened) or opaque content
     * @param charCount total character size of textual content, {@code -1} when not
     *                  computed (structured content is only sized by its kind)
     * @param truncated true when the content exceeded the overview bounds
     * @param textPrefix bounded prefix of textual content, {@code null} otherwise
     */
    public record BoundedMessage(String role, ContentKind contentKind, int charCount, boolean truncated,
                                 String textPrefix) {
    }

    /**
     * Content representation class of one message entry.
     */
    public enum ContentKind {
        /**
         * Textual content, represented by a bounded prefix.
         */
        TEXT,

        /**
         * Structured content (array/object); only its size is reported, it is never
         * flattened into text.
         */
        STRUCTURED,

        /**
         * Missing or non-inspectable content.
         */
        OPAQUE
    }

    /**
     * Which body version a view interprets.
     */
    public enum ViewKind {
        /**
         * The original client request body.
         */
        WIRE,

        /**
         * A mutated body, produced by the component named in {@code mutator}.
         */
        EFFECTIVE
    }
}
