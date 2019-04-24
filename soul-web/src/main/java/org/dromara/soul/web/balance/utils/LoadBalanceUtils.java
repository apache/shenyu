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

package org.dromara.soul.web.balance.utils;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.web.balance.LoadBalance;
import org.dromara.soul.web.balance.factory.LoadBalanceFactory;

import java.util.List;

/**
 * The type Load balance utils.
 *
 * @author xiaoyu(Myth)
 */
public class LoadBalanceUtils {

    /**
     * Selector divide upstream.
     *
     * @param upstreamList the upstream list
     * @param rule         the rule
     * @param ip           the ip
     * @return the divide upstream
     */
    public static DivideUpstream selector(List<DivideUpstream> upstreamList, String rule, String ip) {
        DivideUpstream upstream = null;
        if (upstreamList.size() == 1) {
            upstream = upstreamList.get(0);
        } else {
            if (StringUtils.isNoneBlank(rule)) {
                final LoadBalance loadBalance = LoadBalanceFactory.of(rule);
                upstream = loadBalance.select(upstreamList, ip);
            }
        }
        return upstream;
    }


}
