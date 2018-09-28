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

package org.dromara.soul.bootstrap.redis;

import org.assertj.core.api.Assertions;
import org.dromara.soul.bootstrap.BaseTest;
import org.dromara.soul.common.dto.convert.RateLimiterHandle;
import org.dromara.soul.web.plugin.ratelimter.RateLimiterResponse;
import org.dromara.soul.web.plugin.ratelimter.RedisRateLimiter;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.UUID;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assume.assumeThat;

/**
 * @author xiaoyu
 */
public class RedisRateLimiterTests extends BaseTest {

    @Autowired(required = false)
    private RedisRateLimiter rateLimiter;

    @Test
    public void redisRateLimiterWorks() throws Exception {
        assumeThat("Ignore on Circle",
                System.getenv("CIRCLECI"), is(nullValue()));

        String id = UUID.randomUUID().toString();

        int replenishRate = 1;
        int burstCapacity = 2 * replenishRate;

        RateLimiterHandle handle = new RateLimiterHandle();
        handle.setBurstCapacity(burstCapacity);
        handle.setReplenishRate(replenishRate);

        RateLimiterResponse response;
        // Bursts work
        for (int i = 0; i < 50; i++) {
            response = rateLimiter.isAllowed(id, 0.5, burstCapacity).block();
            Assertions.assertThat(response.isAllowed()).as("Burst # %s is allowed", i).isTrue();
        }


        Thread.sleep(1000);

        // # After the burst is done, check the steady state
        for (int i = 0; i < replenishRate; i++) {
            response = rateLimiter.isAllowed(id, replenishRate, burstCapacity).block();
            assertThat(response.isAllowed()).as("steady  # %s is allowed", i).isTrue();
        }

        response = rateLimiter.isAllowed(id, replenishRate, burstCapacity).block();
        assertThat(response.isAllowed()).as("steady state # %s is allowed", replenishRate).isFalse();
    }

}
