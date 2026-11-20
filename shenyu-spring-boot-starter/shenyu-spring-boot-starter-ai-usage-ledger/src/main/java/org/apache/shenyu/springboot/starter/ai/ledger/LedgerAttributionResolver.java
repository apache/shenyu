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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.ledger.LedgerRecord;

import org.apache.shenyu.common.dto.AgentRequestContext;
import org.apache.shenyu.common.metrics.AiAttemptEvent;
import org.apache.shenyu.common.metrics.AiAttemptRoute;

/** Application-owned trusted mapping. Request headers, payloads and trace IDs are never identity inputs. */
@FunctionalInterface
public interface LedgerAttributionResolver {

    /**
     * Resolve only known facts; null means unknown.
     * @param context trusted invocation snapshot
     * @param provider actual provider key, nullable for logical rows
     * @param clientProtocol actual client protocol, nullable for logical rows
     * @return bounded credential-free attribution
     */
    LedgerRecord.Facts resolve(AgentRequestContext context, String provider, String clientProtocol);

    /**
     * Combine application identity mapping with the actually observed attempt route.
     * @param event frozen attempt facts
     * @return attribution without relabeling the client protocol as the native protocol
     */
    default LedgerRecord.Facts resolve(final AiAttemptEvent event) {
        LedgerRecord.Facts facts = resolve(event.context(), event.provider(), event.protocol());
        AiAttemptRoute route = event.route();
        return new LedgerRecord.Facts(facts.namespace(), facts.tenant(), facts.subject(), facts.application(), facts.feature(), facts.apiKeyId(),
                route.provider(), route.requestedModel(), route.resolvedModel(), route.clientProtocol(), route.targetProtocol(),
                facts.operation(), facts.configurationVersion());
    }

    /**
     * Adapt the current master contract without inventing missing native/model/app metadata.
     * @param namespace explicit deployment-owned namespace; null stays unknown
     * @return default trusted adapter
     */
    static LedgerAttributionResolver fromContext(final String namespace) {
        LedgerRecord.optional(namespace);
        return (context, provider, protocol) -> new LedgerRecord.Facts(namespace, context.getTenant(), context.getSubject(),
                null, null, null, provider, null, context.getTarget(), protocol, null, context.getOperation(), context.getConfigurationVersion());
    }
}
