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
--/

-- Atomic MCP invocation and concurrency admission.
-- KEYS[1] invocation window counter
-- KEYS[2] concurrency lease set
-- ARGV[1] invocation limit
-- ARGV[2] window start epoch second
-- ARGV[3] invocation window TTL seconds
-- ARGV[4] concurrency limit
-- ARGV[5] lease token
-- ARGV[6] lease expiry epoch second
-- ARGV[7] concurrency key TTL seconds
-- ARGV[8] current epoch second
-- Returns {allowed, invocationLimited, concurrencyLimited, lease}
local invocation = tonumber(redis.call('GET', KEYS[1]) or '0')
if invocation >= tonumber(ARGV[1]) then
  return {0, 1, 0, ''}
end
redis.call('ZREMRANGEBYSCORE', KEYS[2], '-inf', ARGV[8])
local concurrency = redis.call('ZCARD', KEYS[2])
if concurrency >= tonumber(ARGV[4]) then
  return {0, 0, 1, ''}
end
redis.call('SET', KEYS[1], invocation + 1, 'EX', ARGV[3])
redis.call('ZADD', KEYS[2], ARGV[6], ARGV[5])
redis.call('EXPIRE', KEYS[2], ARGV[7])
return {1, 0, 0, ARGV[5]}
