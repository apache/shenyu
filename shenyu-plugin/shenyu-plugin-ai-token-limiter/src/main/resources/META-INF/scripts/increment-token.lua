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

-- increment-token.lua
-- KEYS[1]: user's token count key
-- ARGV[1]: increment value
-- ARGV[2]: time window in seconds

local key = KEYS[1]
local increment = tonumber(ARGV[1])
local ttl = tonumber(ARGV[2])
local currentTokens = redis.call('GET', key)

if currentTokens then
  redis.call('INCRBY', key, increment)
  redis.call('EXPIRE', key, ttl)  -- fresh the expiration time
else
  redis.call('SETEX', key, ttl, increment)  -- set initial value and expiration time
end

return redis.call('GET', key)  -- return the current token count
