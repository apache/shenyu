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

package org.apache.shenyu.e2e.client;

import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.model.Plugin;
import org.apache.shenyu.e2e.model.data.Condition;
import org.apache.shenyu.e2e.model.data.MetaData;
import org.junit.jupiter.api.Assertions;
import org.opentest4j.AssertionFailedError;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Map;

import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newConditions;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newDivideRuleHandle;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newRuleBuilder;
import static org.apache.shenyu.e2e.template.ResourceDataTemplate.newSelectorBuilder;

/**
 * WaitDataSync.
 */
public class WaitDataSync {

    private static final Logger LOGGER = LoggerFactory.getLogger(WaitDataSync.class);

    /**
     * Verify sync with explicit Admin-owned resources in a fresh test stack.
     *
     * @param admin Admin test client
     * @param gateway gateway test client
     * @param rpcType RPC type of the metadata under test
     * @throws Exception if sync fails
     */
    public static void verifyFixtureData(final AdminClient admin, final GatewayClient gateway, final String rpcType) throws Exception {
        admin.login();
        final String name = rpcType + "-fixture-sync";
        final String context = "/" + name;
        final String path = context + "/metadata";
        final var conditions = newConditions(Condition.ParamType.URI, Condition.Operator.EQUAL, path);
        final var selector = admin.create(newSelectorBuilder(name, Plugin.DIVIDE).conditionList(conditions).build());
        final var rule = newRuleBuilder(name).conditionList(conditions).handle(newDivideRuleHandle()).build();
        rule.setSelectorId(selector.getId());
        admin.create(rule);
        admin.createMetaData(MetaData.builder()
                .appName(name).contextPath(context).path(path).rpcType(rpcType)
                .serviceName(name).methodName("metadata").enabled(true).build());
        waitAdmin2GatewayDataSyncEquals(admin::listAllSelectors, gateway::getSelectorCache, admin);
        waitAdmin2GatewayDataSyncEquals(admin::listAllRules, gateway::getRuleCache, admin);
        waitAdmin2GatewayDataSyncEquals(admin::listAllMetaData, gateway::getMetaDataCache, admin);
        Assertions.assertTrue(gateway.getMetaDataCache().stream().anyMatch(metadata -> path.equals(metadata.getPath())),
                "Fixture metadata is visible in the gateway");
    }

    /**
     * waitAdmin2GatewayDataSync.
     *
     * @param adminSupplier adminSupplier
     * @param gatewaySupplier gatewaySupplier
     * @param adminClient adminClient
     * @param <T> List
     * @param <U> List
     * @throws Exception Exception
     */
    public static <T extends List<?>, U extends List<?>> void waitAdmin2GatewayDataSyncEquals(final ShenyuE2ESupplier<T> adminSupplier,
                                                 final ShenyuE2ESupplier<U> gatewaySupplier, final AdminClient adminClient) throws Exception {
        final String testClassName = Thread.currentThread().getStackTrace()[2].getClassName();
        int retryNum = 0;
        List<?> gatewayDataList = null;
        List<?> adminDataList = adminSupplier.get();
        if (CollectionUtils.isEmpty(adminDataList)) {
            LOGGER.warn("dataSyncEquals test {} adminDataList size is zero sleep 10s... ", testClassName);
            Thread.sleep(10000);
        }
        while (retryNum < 12) {
            adminDataList = adminSupplier.get();
            gatewayDataList = gatewaySupplier.get();
            LOGGER.warn("dataSyncEquals test {} admin size = {} gateway size = {}", testClassName, adminDataList.size(), gatewayDataList.size());
            if (!CollectionUtils.isEmpty(adminDataList) && gatewayDataList.size() == adminDataList.size()) {
                break;
            }
            if (retryNum % 3 == 0) {
                adminClient.syncPluginAll();
            }
            Thread.sleep(10000);
            retryNum++;
        }
        Assertions.assertNotEquals(adminDataList.size(), 0);
        Assertions.assertEquals(adminDataList.size(), gatewayDataList.size());
    }

    /**
     * waitGatewayPluginUse.
     *
     * @param gatewayClient gatewayClient
     * @param pluginClass AbstractShenyuPlugin implement class
     * @throws Exception Exception
     */
    public static void waitGatewayPluginUse(final GatewayClient gatewayClient, final String pluginClass) throws Exception {
        Map<String, Integer> pluginMap = gatewayClient.getPlugins();
        int retryNum = 0;
        boolean existPlugin = false;
        while (!existPlugin && retryNum < 10) {
            for (String plugin : pluginMap.keySet()) {
                if (plugin.startsWith(pluginClass)) {
                    existPlugin = true;
                    break;
                }
            }
            Thread.sleep(10000);
            retryNum++;
            pluginMap = gatewayClient.getPlugins();
        }
        if (!existPlugin) {
            throw new AssertionFailedError(pluginClass + " plugin not found");
        }
    }

    @FunctionalInterface
    public interface ShenyuE2ESupplier<T> {

        /**
         * get.
         *
         * @return {@link T}
         * @throws Exception Exception
         */
        T get() throws Exception;
    }
}
