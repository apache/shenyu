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
