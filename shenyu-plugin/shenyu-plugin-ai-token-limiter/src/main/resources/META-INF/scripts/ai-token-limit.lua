-- token_rate_limit.lua
-- KEYS[1]: unique identifier for rate limiting
-- ARGV[1]: maximum request count
-- ARGV[2]: time window in seconds
-- ARGV[3]: current timestamp in milliseconds
-- Returns: 1 if request is allowed, 0 if rejected

local key = KEYS[1]
local max_requests = tonumber(ARGV[1])
local window_seconds = tonumber(ARGV[2])
local current_time = tonumber(ARGV[3])

-- Calculate the start time of current window (in milliseconds)
local window_start_time
if window_seconds == 1 then
    -- Second-level window: rounded to second
    window_start_time = math.floor(current_time / 1000) * 1000
elseif window_seconds == 60 then
    -- Minute-level window: rounded to minute
    window_start_time = math.floor(current_time / (60 * 1000)) * (60 * 1000)
elseif window_seconds == 3600 then
    -- Hour-level window: rounded to hour
    window_start_time = math.floor(current_time / (3600 * 1000)) * (3600 * 1000)
elseif window_seconds == 86400 then
    -- Day-level window: rounded to day
    window_start_time = math.floor(current_time / (86400 * 1000)) * (86400 * 1000)
end

-- Create a sorted set with score as timestamp and member as timestamp:counter
local counter_key = key .. ":" .. tostring(window_start_time)

-- Remove expired window data (optional, can also rely on Redis expiration)
redis.call('ZREMRANGEBYSCORE', key, 0, window_start_time - 1)

-- Get current request count in the window
local current_count = redis.call('ZCARD', counter_key)

-- If under limit, increment counter and allow request
if current_count < max_requests then
    -- Add current request to sorted set
    local member = tostring(current_time) .. ":" .. tostring(current_count + 1)
    redis.call('ZADD', counter_key, current_time, member)
    
    -- Set expiration to ensure automatic cleanup
    redis.call('EXPIRE', counter_key, window_seconds)
    
    return 1
else
    -- Reject request
    return 0
end
