/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.balance.factory;

import org.dromara.soul.common.utils.SpiLoadFactory;
import org.dromara.soul.web.balance.LoadBalance;
import org.dromara.soul.web.balance.spi.RandomLoadBalance;

import java.util.Objects;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

/**
 * LoadBalanceFactory.
 *
 * @author xiaoyu(Myth)
 */
public class LoadBalanceFactory {

    private static final ServiceLoader<LoadBalance> SERVICE_LOADER =
            SpiLoadFactory.loadAll(LoadBalance.class);

    /**
     * factory of .
     *
     * @param algorithm param
     * @return LoadBalance load balance
     */
    public static LoadBalance of(final String algorithm) {
        return StreamSupport.stream(SERVICE_LOADER.spliterator(), false)
                .filter(service -> Objects.equals(service.algorithm(), algorithm))
                .findFirst().orElseGet(RandomLoadBalance::new);
    }
}
