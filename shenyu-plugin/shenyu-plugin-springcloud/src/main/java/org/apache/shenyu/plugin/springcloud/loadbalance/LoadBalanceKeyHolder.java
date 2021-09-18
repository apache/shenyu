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

package org.apache.shenyu.plugin.springcloud.loadbalance;

import org.springframework.core.NamedThreadLocal;

import java.util.Objects;

public class LoadBalanceKeyHolder {

    private static final ThreadLocal<LoadBalanceKey> HOLDER = new NamedThreadLocal<>(LoadBalanceKey.class.getName());

    /**
     * Reset the LoadBalanceKey for the current thread.
     */
    public static void resetLoadBalanceKey() {
        HOLDER.remove();
    }

    /**
     * Associate the given LoadBalanceKey with the current thread.
     *
     * @param loadBalanceKey the current loadBalanceKey
     */
    public static void setLoadBalanceKey(final LoadBalanceKey loadBalanceKey) {
        if (Objects.isNull(loadBalanceKey)) {
            resetLoadBalanceKey();
        } else {
            HOLDER.set(loadBalanceKey);
        }
    }

    /**
     * Return the LoadBalanceKey associated with the current thread, if any.
     *
     * @return the current LoadBalanceKey
     */
    public static LoadBalanceKey getLoadBalanceKey() {
        return HOLDER.get();
    }
}
