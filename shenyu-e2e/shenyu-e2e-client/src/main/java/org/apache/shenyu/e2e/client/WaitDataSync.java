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

import com.fasterxml.jackson.core.JsonProcessingException;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.model.data.RuleCacheData;
import org.apache.shenyu.e2e.model.response.RuleDTO;
import org.junit.jupiter.api.Assertions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * WaitDataSync.
 */
public class WaitDataSync {

    private static final Logger LOGGER = LoggerFactory.getLogger(WaitDataSync.class);

    /**
     * waitAdmin2GatewayDataSync.
     *
     * @param adminClient adminClient
     * @param gatewayClient gatewayClient
     * @throws InterruptedException InterruptedException
     * @throws JsonProcessingException JsonProcessingException
     */
    public static void waitAdmin2GatewayDataSync(final AdminClient adminClient,
                                                 final GatewayClient gatewayClient) throws InterruptedException, JsonProcessingException {
        int retryNum = 0;
        List<RuleCacheData> ruleCacheList = null;
        List<RuleDTO> ruleDTOList = null;
        while (retryNum < 10) {
            Thread.sleep(10000);
            ruleCacheList = gatewayClient.getRuleCache();
            ruleDTOList = adminClient.listAllRules();
            LOGGER.info("waitAdmin2GatewayDataSync debug admin rule size = {} , gateway rule size = {}", ruleDTOList.size(), ruleCacheList.size());
            if (ruleCacheList.size() == ruleDTOList.size()) {
                break;
            }
            retryNum++;
        }
        Assertions.assertEquals(ruleDTOList.size(), ruleCacheList.size());
    }
}
