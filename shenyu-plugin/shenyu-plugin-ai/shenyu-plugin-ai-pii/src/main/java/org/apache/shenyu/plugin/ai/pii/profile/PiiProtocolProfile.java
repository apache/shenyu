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

package org.apache.shenyu.plugin.ai.pii.profile;

import com.google.gson.JsonObject;

import java.util.List;

/**
 * The text fields of one AI protocol payload. A profile only reports the
 * message text fields of the protocol, so a redaction never touches tool
 * definitions, function arguments or other nontext members of the document.
 */
public interface PiiProtocolProfile {

    /**
     * The profile identifier, aligned with the protocol names the AI proxy
     * already uses, for example openai-chat.
     *
     * @return the profile id
     */
    String id();

    /**
     * The text slots of a request payload of the protocol.
     *
     * @param root the parsed request body
     * @return the slots, empty when the payload carries no message text
     */
    List<PiiTextSlot> requestSlots(JsonObject root);

    /**
     * The text slots of a non streaming response payload of the protocol.
     *
     * @param root the parsed response body
     * @return the slots, empty when the payload carries no message text
     */
    List<PiiTextSlot> responseSlots(JsonObject root);
}
