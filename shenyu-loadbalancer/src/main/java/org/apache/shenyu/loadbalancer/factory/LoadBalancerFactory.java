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

package org.apache.shenyu.loadbalancer.factory;

import java.util.List;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.spi.LoadBalancer;
import org.apache.shenyu.spi.ExtensionLoader;

/**
 * The type Load balance Factory.
 */
public final class LoadBalancerFactory {

    private LoadBalancerFactory() {
    }

    /**
     * Selector upstream.
     *
     * @param upstreamList the upstream list
     * @param algorithm    the loadBalance algorithm
     * @param ip           the ip
     * @return the upstream
     */
    public static Upstream selector(final List<Upstream> upstreamList, final String algorithm, final String ip) {
        LoadBalancer loadBalance = ExtensionLoader.getExtensionLoader(LoadBalancer.class).getJoin(algorithm);
        return loadBalance.select(upstreamList, ip);
    }
}
