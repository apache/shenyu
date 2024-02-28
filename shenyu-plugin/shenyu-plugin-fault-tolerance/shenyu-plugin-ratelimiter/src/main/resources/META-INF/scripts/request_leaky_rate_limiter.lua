--
-- Licensed to the Apache Software Foundation (ASF) under one or more
-- contributor license agreements.  See the NOTICE file distributed with
-- this work for additional information regarding copyright ownership.
-- The ASF licenses this file to You under the Apache License, Version 2.0
-- (the "License"); you may not use this file except in compliance with
-- the License.  You may obtain a copy of the License at
--
--    http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

-- author Redick01

-- current key
local leaky_bucket_key = KEYS[1]
-- last update key
local last_bucket_key = KEYS[2]
-- capacity
local capacity = tonumber(ARGV[2])
-- the rate of leak water
local rate = tonumber(ARGV[1])
-- request count
local requested = tonumber(ARGV[4])
-- current timestamp
local now = tonumber(ARGV[3])
-- the key life time
local key_lifetime = math.ceil((capacity / rate) + 1)


-- the yield of water in the bucket default 0
local key_bucket_count = tonumber(redis.call("GET", leaky_bucket_key)) or 0

-- the last update time default now
local last_time = tonumber(redis.call("GET", last_bucket_key)) or now

-- the time difference
local millis_since_last_leak = now - last_time

-- the yield of water had lasted
local leaks = millis_since_last_leak * rate

if leaks > 0 then
    -- clean up the bucket
    if leaks >= key_bucket_count then
        key_bucket_count = 0
    else
        -- compute the yield of water in the bucket
        key_bucket_count = key_bucket_count - leaks
    end
    last_time = now
end

-- is allowed pass default not allow
local is_allow = 0

local new_bucket_count = key_bucket_count + requested
-- allow
if new_bucket_count <= capacity then
    is_allow = 1
else
    -- not allow
    return {is_allow, new_bucket_count}
end

-- update the key bucket water yield
redis.call("SETEX", leaky_bucket_key, key_lifetime, new_bucket_count)

-- update last update time
redis.call("SETEX", last_bucket_key, key_lifetime, now)

-- return
return {is_allow, new_bucket_count}