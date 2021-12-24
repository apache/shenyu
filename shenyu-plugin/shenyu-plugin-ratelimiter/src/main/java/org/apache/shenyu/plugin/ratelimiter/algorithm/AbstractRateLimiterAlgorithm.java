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

import org.apache.shenyu.common.constant.Constants;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.scripting.support.ResourceScriptSource;

import java.util.Arrays;
import java.util.List;

/**
 * The type Abstract rate limiter algorithm.
 */
public abstract class AbstractRateLimiterAlgorithm implements RateLimiterAlgorithm<List<Long>> {

    private final String scriptName;

    private final RedisScript<List<Long>> script;
    
    @SuppressWarnings({"unchecked", "rawtypes"})
    protected AbstractRateLimiterAlgorithm(final String scriptName) {
        DefaultRedisScript redisScript = new DefaultRedisScript<>();
        String scriptPath = Constants.SCRIPT_PATH + scriptName;
        redisScript.setScriptSource(new ResourceScriptSource(new ClassPathResource(scriptPath)));
        redisScript.setResultType(List.class);
        this.script = redisScript;
        this.scriptName = scriptName;
    }
    
    /**
     * Gets key name.
     *
     * @return the key name
     */
    protected abstract String getKeyName();

    @Override
    public String getScriptName() {
        return scriptName;
    }

    @Override
    public RedisScript<List<Long>> getScript() {
        return script;
    }

    @Override
    public List<String> getKeys(final String id) {
        String prefix = getKeyName() + ".{" + id;
        String tokenKey = prefix + "}.tokens";
        String timestampKey = prefix + "}.timestamp";
        return Arrays.asList(tokenKey, timestampKey);
    }
}
