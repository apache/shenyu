--
-- Licensed to the Apache Software Foundation (ASF) under one or more
-- contributor license agreements.  See the NOTICE file distributed with
-- this work for additional information regarding copyright ownership.
-- The ASF licenses this file to You under the Apache License, Version 2.0
-- (the "License"); you may not use this file except in compliance with
-- the License.  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

-- Atomic MCP invocation and concurrency admission.
-- All keys share the mode/tenant hash tag, including the tenant aggregate.
-- KEYS[1] subject window hash
-- KEYS[2] concurrency lease set
-- KEYS[3] tenant window hash
-- ARGV[1] invocation limit
-- ARGV[2] window milliseconds
-- ARGV[3] subject concurrency limit
-- ARGV[4] lease token
-- ARGV[5] lease TTL milliseconds
-- Returns {allowed, invocationLimited, concurrencyLimited, lease}
local clock = redis.call('TIME')
local now = tonumber(clock[1]) * 1000 + math.floor(tonumber(clock[2]) / 1000)
local windowSize = tonumber(ARGV[2])
local window = math.floor(now / windowSize) * windowSize
local function count(key)
  if tonumber(redis.call('HGET', key, 'window')) ~= window then
    return 0
  end
  return tonumber(redis.call('HGET', key, 'count') or '0')
end
local subjectCount = count(KEYS[1])
local tenantCount = count(KEYS[3])
if subjectCount >= tonumber(ARGV[1]) or tenantCount >= tonumber(ARGV[1]) then
  return {0, 1, 0, ''}
end
redis.call('ZREMRANGEBYSCORE', KEYS[2], '-inf', now)
local concurrency = redis.call('ZCARD', KEYS[2])
if concurrency >= tonumber(ARGV[3]) then
  return {0, 0, 1, ''}
end
local ttl = window + windowSize - now + 1
redis.call('HSET', KEYS[1], 'window', window, 'count', subjectCount + 1)
redis.call('PEXPIRE', KEYS[1], ttl)
redis.call('HSET', KEYS[3], 'window', window, 'count', tenantCount + 1)
redis.call('PEXPIRE', KEYS[3], ttl)
redis.call('ZADD', KEYS[2], now + tonumber(ARGV[5]), ARGV[4])
redis.call('PEXPIRE', KEYS[2], math.max(redis.call('PTTL', KEYS[2]), tonumber(ARGV[5]) + 1))
return {1, 0, 0, ARGV[4]}
