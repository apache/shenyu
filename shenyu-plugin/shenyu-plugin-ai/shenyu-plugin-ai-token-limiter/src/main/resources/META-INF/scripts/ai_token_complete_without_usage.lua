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

-- Complete a request without reported usage, retaining its original window TTL.
-- KEYS[1]: original scope/window hash; ARGV: request ID, outcome
-- Outcomes: NOT_FORWARDED (caller proves no upstream execution), USAGE_UNKNOWN
-- Returns: 0 = reservation missing, 1 = released, 2 = unknown usage held,
--          3 = already settled, 4 = window missing

local key = KEYS[1]
local request_id = ARGV[1]
local outcome = ARGV[2]
local max_integer = 9007199254740991

local function is_integer(value, minimum)
    return value and value >= minimum and value <= max_integer and value == math.floor(value)
end

if not key or #KEYS ~= 1 or #ARGV ~= 2 or not request_id or request_id == ''
        or (outcome ~= 'NOT_FORWARDED' and outcome ~= 'USAGE_UNKNOWN') then
    return redis.error_reply('Invalid completion arguments')
end

local remaining_ttl = redis.call('PTTL', key)
if remaining_ttl == -2 then
    return 4
end
if remaining_ttl == -1 then
    return redis.error_reply('Budget window must have an expiry')
end

local reservation = redis.call('HGET', key, 'r:' .. request_id)
local settled = redis.call('HGET', key, 's:' .. request_id)
local completion_field = 'f:' .. request_id
local completion = redis.call('HGET', key, completion_field)
if not reservation then
    if settled or completion then
        return redis.error_reply('Completion has no original reservation')
    end
    return 0
end

local amount = tonumber(reservation)
local used = tonumber(redis.call('HGET', key, 'used'))
local reserved = tonumber(redis.call('HGET', key, 'reserved'))
if not is_integer(amount, 1) or not is_integer(used, 0) or not is_integer(reserved, 0)
        or (completion and completion ~= 'NOT_FORWARDED' and completion ~= 'USAGE_UNKNOWN') then
    return redis.error_reply('Invalid budget state')
end
if completion == 'NOT_FORWARDED' and (not settled or tonumber(settled) ~= 0) then
    return redis.error_reply('Released request must have zero usage')
end

if settled then
    if not is_integer(tonumber(settled), 0) then
        return redis.error_reply('Invalid settlement state')
    end
    if completion == 'NOT_FORWARDED' then
        return 1
    end
    return 3
end

-- Unknown usage may later be reconciled with reliable usage, but never freed
-- by an assertion that the request was not forwarded.
if completion == 'USAGE_UNKNOWN' then
    if outcome == 'NOT_FORWARDED' then
        return redis.error_reply('Unknown upstream usage cannot be released as not forwarded')
    end
    return 2
end
if reserved < amount then
    return redis.error_reply('Reserved total is smaller than the request reservation')
end

if outcome == 'USAGE_UNKNOWN' then
    redis.call('HSET', key, completion_field, outcome)
    return 2
end

redis.call('HSET', key,
        'reserved', string.format('%.0f', reserved - amount),
        's:' .. request_id, '0',
        completion_field, outcome)
return 1
