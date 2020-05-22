local key = KEYS[1]

local capacity = tonumber(ARGV[1])
local timestamp = tonumber(ARGV[2])
local id = ARGV[3]

local count = redis.call("zcard", key)
local allowed = count < capacity

if allowed then
  redis.call("zadd", key, timestamp, id)
end
redis.call("setex", key, timestamp)
return { allowed, count }