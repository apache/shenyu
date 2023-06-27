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

package org.apache.shenyu.plugin.ratelimiter.algorithm;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.scripting.support.ResourceScriptSource;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

/**
 * test for AbstractRateLimiterAlgorithm.
 */
@ExtendWith(MockitoExtension.class)
public final class AbstractRateLimiterAlgorithmTest {

    private static final String RATE_ALGORITHM_SCRIPT_NAME = "request_leaky_rate_limiter.lua";

    private static final String KEY_NAME = "request_leaky_rate_limiter";

    private static final String ID = "1";

    private AbstractRateLimiterAlgorithm abstractRateLimiterAlgorithm;

    @BeforeEach
    public void startup() {
        abstractRateLimiterAlgorithm = new AbstractRateLimiterAlgorithm(RATE_ALGORITHM_SCRIPT_NAME) {
            @Override
            public String getKeyName() {
                return KEY_NAME;
            }
        };
    }

    @Test
    public void getScriptTest() {
        DefaultRedisScript<List> redisScript = new DefaultRedisScript<>();
        String scriptPath = "/META-INF/scripts/" + abstractRateLimiterAlgorithm.getScriptName();
        redisScript.setScriptSource(new ResourceScriptSource(new ClassPathResource(scriptPath)));
        redisScript.setResultType(List.class);
        assertThat(redisScript.getScriptAsString(), is(abstractRateLimiterAlgorithm.getScript().getScriptAsString()));
        assertThat(redisScript.getResultType(), is(abstractRateLimiterAlgorithm.getScript().getResultType()));
    }

    @Test
    public void getKeysTest() {
        String prefix = abstractRateLimiterAlgorithm.getKeyName() + ".{" + ID;
        String tokenKey = prefix + "}.tokens";
        String timestampKey = prefix + "}.timestamp";
        List<String> keys = abstractRateLimiterAlgorithm.getKeys(ID);
        assertThat(tokenKey, is(keys.get(0)));
        assertThat(timestampKey, is(keys.get(1)));
    }
}
