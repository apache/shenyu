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

-- Atomically reserve capacity for an immutable budget window.
-- KEYS[1]: budget hash for one scope and one window
-- ARGV: limit, reservation amount, remaining window TTL in ms, request ID
-- Optional ARGV[5..6]: inclusive start and exclusive end in epoch milliseconds.
-- When provided, TTL is the full window size and expiry is fixed at its end.
-- Returns: 0 = rejected, 1 = newly reserved, 2 = already reserved,
--          3 = already settled, 4 = already released, 5 = unknown usage held,
--          6 = window is no longer current (no state written)

local key = KEYS[1]
local limit = tonumber(ARGV[1])
local amount = tonumber(ARGV[2])
local ttl = tonumber(ARGV[3])
local request_id = ARGV[4]
local max_integer = 9007199254740991

local function is_integer(value, minimum)
    return value and value >= minimum and value <= max_integer and value == math.floor(value)
end

if not key or #KEYS ~= 1 or (#ARGV ~= 4 and #ARGV ~= 6)
        or not is_integer(limit, 1) or not is_integer(amount, 1)
        or not is_integer(ttl, 1) or not request_id or request_id == '' then
    return redis.error_reply('Invalid reservation arguments')
end

local window_end
if #ARGV == 6 then
    local window_start = tonumber(ARGV[5])
    window_end = tonumber(ARGV[6])
    if not is_integer(window_start, 0) or not is_integer(window_end, 1)
            or window_end <= window_start or window_end - window_start ~= ttl then
        return redis.error_reply('Invalid fixed window boundaries')
    end
    local time = redis.call('TIME')
    local now = tonumber(time[1]) * 1000 + math.floor(tonumber(time[2]) / 1000)
    if now < window_start or now >= window_end then
        return 6
    end
end

local remaining_ttl = redis.call('PTTL', key)
if remaining_ttl == -1 then
    return redis.error_reply('Budget window must have an expiry')
end

local used_value = redis.call('HGET', key, 'used')
local reserved_value = redis.call('HGET', key, 'reserved')
if remaining_ttl ~= -2 and (not used_value or not reserved_value) then
    return redis.error_reply('Existing budget window is missing counters')
end
local used = tonumber(used_value or '0')
local reserved = tonumber(reserved_value or '0')
if not is_integer(used, 0) or not is_integer(reserved, 0) then
    return redis.error_reply('Invalid budget state')
end

local reservation_field = 'r:' .. request_id
local existing = redis.call('HGET', key, reservation_field)
local settled = redis.call('HGET', key, 's:' .. request_id)
local completion = redis.call('HGET', key, 'f:' .. request_id)
if completion and completion ~= 'NOT_FORWARDED' and completion ~= 'USAGE_UNKNOWN' then
    return redis.error_reply('Invalid completion state')
end
if completion == 'NOT_FORWARDED' and (not settled or tonumber(settled) ~= 0) then
    return redis.error_reply('Released request must have zero usage')
end
if existing then
    if tonumber(existing) ~= amount then
        return redis.error_reply('Request ID already has a different reservation amount')
    end
    if settled then
        if not is_integer(tonumber(settled), 0) then
            return redis.error_reply('Invalid settlement state')
        end
        if completion == 'NOT_FORWARDED' then
            return 4
        end
        return 3
    end
    if completion == 'USAGE_UNKNOWN' then
        return 5
    end
    return 2
end
if settled or completion then
    return redis.error_reply('Completion has no original reservation')
end

-- Subtraction avoids overflowing an exact Lua integer during the comparison.
if used > limit or reserved > limit - used or amount > limit - used - reserved then
    return 0
end

redis.call('HSET', key,
        'used', string.format('%.0f', used),
        'reserved', string.format('%.0f', reserved + amount),
        reservation_field, string.format('%.0f', amount))
if remaining_ttl == -2 then
    if window_end then
        redis.call('PEXPIREAT', key, string.format('%.0f', window_end))
    else
        redis.call('PEXPIRE', key, string.format('%.0f', ttl))
    end
end

return 1
