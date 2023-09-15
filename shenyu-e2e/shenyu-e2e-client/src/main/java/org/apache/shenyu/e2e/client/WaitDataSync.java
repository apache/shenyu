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
     * @param adminSupplier adminSupplier
     * @param gatewaySupplier gatewaySupplier
     * @param adminClient adminClient
     * @param <T> List
     * @param <U> List
     * @throws Exception Exception
     */
    public static <T extends List<?>, U extends List<?>> void waitAdmin2GatewayDataSyncEquals(final ShenyuE2ESupplier<T> adminSupplier,
                                                 final ShenyuE2ESupplier<U> gatewaySupplier, final AdminClient adminClient) throws Exception {
        int retryNum = 0;
        List<?> gatewayDataList = null;
        List<?> adminDataList = adminSupplier.get();
        if (adminDataList != null && adminDataList.isEmpty()) {
            Thread.sleep(10000);
        }
        while (retryNum < 10) {
            adminDataList = adminSupplier.get();
            gatewayDataList = gatewaySupplier.get();
            LOGGER.warn("waitAdmin2GatewayDataSyncEquals debug admin rule size = {} , gateway rule size = {}", adminDataList.size(), gatewayDataList.size());
            if (gatewayDataList.size() == adminDataList.size()) {
                break;
            }
            adminClient.syncPluginAll();
            Thread.sleep(3000);
            retryNum++;
        }
        Assertions.assertNotEquals(adminDataList.size(), 0);
        Assertions.assertEquals(adminDataList.size(), gatewayDataList.size());
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
