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

package org.apache.shenyu.springboot.starter.plugin.jwe;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

import java.util.List;
import java.util.Map;

/** Deployment-local JWE policy; configuration references key beans, not serialized private keys. */
@ConfigurationProperties(prefix = "shenyu.jwe-payload", ignoreUnknownFields = false)
public record JwePayloadProperties(@DefaultValue("1048576") int maxCiphertextBytes, @DefaultValue("1048576") int maxPlaintextBytes, List<Route> routes) {

    /** One namespace/rule key ring and optional active response recipient. */
    public record Route(String namespaceId, String ruleId, Map<String, String> decryptionKeys, String recipientKeyId, String recipientKeyBean) {
    }
}
