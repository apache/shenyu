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

-- Reconcile one reservation with known final usage without changing the TTL.
-- KEYS[1]: the original scope/window budget hash
-- ARGV: actual usage, request ID
-- Returns: 0 = reservation missing, 1 = settled, 2 = already settled,
--          3 = window missing

local key = KEYS[1]
local actual = tonumber(ARGV[1])
local request_id = ARGV[2]
local max_integer = 9007199254740991

local function is_integer(value, minimum)
    return value and value >= minimum and value <= max_integer and value == math.floor(value)
end

if not key or #KEYS ~= 1 or #ARGV ~= 2
        or not is_integer(actual, 0) or not request_id or request_id == '' then
    return redis.error_reply('Invalid settlement arguments')
end

local remaining_ttl = redis.call('PTTL', key)
if remaining_ttl == -2 then
    return 3
end
if remaining_ttl == -1 then
    return redis.error_reply('Budget window must have an expiry')
end

local reservation_field = 'r:' .. request_id
local settlement_field = 's:' .. request_id
local reservation = redis.call('HGET', key, reservation_field)
local settled = redis.call('HGET', key, settlement_field)
local completion = redis.call('HGET', key, 'f:' .. request_id)
if not reservation then
    if settled or completion then
        return redis.error_reply('Completion has no original reservation')
    end
    return 0
end

local amount = tonumber(reservation)
local used = tonumber(redis.call('HGET', key, 'used'))
local reserved = tonumber(redis.call('HGET', key, 'reserved'))
if not is_integer(amount, 1) or not is_integer(used, 0) or not is_integer(reserved, 0) then
    return redis.error_reply('Invalid budget state')
end
if completion and completion ~= 'NOT_FORWARDED' and completion ~= 'USAGE_UNKNOWN' then
    return redis.error_reply('Invalid completion state')
end
if completion == 'NOT_FORWARDED' and (not settled or tonumber(settled) ~= 0) then
    return redis.error_reply('Released request must have zero usage')
end

if settled then
    if not is_integer(tonumber(settled), 0) or tonumber(settled) ~= actual then
        return redis.error_reply('Request ID already has a different settlement')
    end
    return 2
end

-- Validate all state before writing; actual usage may exceed the original budget.
if reserved < amount then
    return redis.error_reply('Reserved total is smaller than the request reservation')
end
if actual > max_integer - used then
    return redis.error_reply('Settled usage exceeds the exact Lua integer range')
end

redis.call('HSET', key,
        'used', string.format('%.0f', used + actual),
        'reserved', string.format('%.0f', reserved - amount),
        settlement_field, string.format('%.0f', actual))

return 1
