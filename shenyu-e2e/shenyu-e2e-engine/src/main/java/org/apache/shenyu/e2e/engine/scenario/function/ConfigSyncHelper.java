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

package org.apache.shenyu.e2e.engine.scenario.function;

import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;
import org.apache.shenyu.e2e.model.data.MetaData;
import org.apache.shenyu.e2e.model.data.RuleCacheData;
import org.apache.shenyu.e2e.model.data.SelectorCacheData;

import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newDivideRuleHandle;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;
import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Observe configuration convergence for the resources owned by one scenario.
 */
public final class ConfigSyncHelper {

    private static final Duration TIMEOUT = Duration.ofSeconds(30);

    private ConfigSyncHelper() {
    }

    /**
     * Verify selectors, rules and metadata reach the gateway and clean up the test data.
     *
     * @param admin Admin client
     * @param gateway gateway client
     * @param rpcType RPC type for the metadata
     */
    public static void verifyResourceSync(final AdminClient admin, final GatewayClient gateway, final String rpcType) {
        admin.login();
        final String name = rpcType + "-fixture-sync";
        final var conditions = newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, "/" + name);
        final String selectorId = admin.create(newSelectorBuilder(name, Plugin.DIVIDE).conditionList(conditions).build()).getId();
        final String context = "/" + name;
        final String metadataPath = context + "/" + selectorId;
        final List<String> ruleIds = new ArrayList<>(1);
        try {
            final var rule = newRuleBuilder(name).conditionList(conditions).handle(newDivideRuleHandle()).build();
            rule.setSelectorId(selectorId);
            ruleIds.add(admin.create(rule).getId());
            admin.createMetaData(MetaData.builder()
                    .appName(name).contextPath(context).path(metadataPath).rpcType(rpcType)
                    .serviceName(name).methodName("metadata").enabled(true).build());
            awaitCreated(gateway, List.of(selectorId), ruleIds);
            await().alias("Created metadata applied to gateway: " + metadataPath).atMost(TIMEOUT)
                    .until(() -> gateway.getMetaDataCache().stream()
                            .anyMatch(metadata -> metadataPath.equals(metadata.getPath()) && rpcType.equals(metadata.getRpcType())));
        } finally {
            assertAll("Clean up temporary synchronization resources",
                    () -> admin.deleteMetaDataByPath(metadataPath),
                    () -> admin.deleteRules(ruleIds.toArray(String[]::new)),
                    () -> admin.deleteSelectors(selectorId));
        }
        awaitDeleted(gateway, List.of(selectorId), ruleIds);
    }

    /**
     * Wait until all newly created selectors and rules are present in the gateway.
     * @param gateway gateway client
     * @param selectors created selector identifiers
     * @param rules created rule identifiers
     */
    public static void awaitCreated(final GatewayClient gateway, final List<String> selectors, final List<String> rules) {
        awaitCreated(gateway, selectors, rules, TIMEOUT);
    }

    static void awaitCreated(final GatewayClient gateway, final List<String> selectors, final List<String> rules, final Duration timeout) {
        await().alias("Created selectors and rules applied to gateway")
                .atMost(timeout).pollDelay(Duration.ZERO).pollInterval(Duration.ofMillis(100))
                .untilAsserted(() -> {
                    Set<String> observedSelectors = gateway.getSelectorCache().stream().map(SelectorCacheData::getId).collect(Collectors.toSet());
                    Set<String> observedRules = gateway.getRuleCache().stream().map(RuleCacheData::getId).collect(Collectors.toSet());
                    assertTrue(observedSelectors.containsAll(selectors), () -> "Expected selectors " + selectors + ", observed " + observedSelectors);
                    assertTrue(observedRules.containsAll(rules), () -> "Expected rules " + rules + ", observed " + observedRules);
                });
    }

    /**
     * Wait until the deleted selectors and rules have left the gateway.
     * @param gateway gateway client
     * @param selectors deleted selector identifiers
     * @param rules deleted rule identifiers
     */
    public static void awaitDeleted(final GatewayClient gateway, final List<String> selectors, final List<String> rules) {
        awaitDeleted(gateway, selectors, rules, TIMEOUT);
    }

    static void awaitDeleted(final GatewayClient gateway, final List<String> selectors, final List<String> rules, final Duration timeout) {
        await().alias("Deleted selectors and rules removed from gateway")
                .atMost(timeout).pollDelay(Duration.ZERO).pollInterval(Duration.ofMillis(100))
                .untilAsserted(() -> {
                    Set<String> observedSelectors = gateway.getSelectorCache().stream().map(SelectorCacheData::getId).collect(Collectors.toSet());
                    Set<String> observedRules = gateway.getRuleCache().stream().map(RuleCacheData::getId).collect(Collectors.toSet());
                    assertTrue(Collections.disjoint(observedSelectors, selectors), () -> "Deleted selectors " + selectors + ", observed " + observedSelectors);
                    assertTrue(Collections.disjoint(observedRules, rules), () -> "Deleted rules " + rules + ", observed " + observedRules);
                });
    }
}
