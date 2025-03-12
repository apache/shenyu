-- check-limit.lua
-- KEYS[1]: user's token count key
-- ARGV[1]: the limit of tokens

local key = KEYS[1]
local currentTokens = redis.call('GET', key)
if currentTokens and tonumber(currentTokens) >= tonumber(ARGV[1]) then
  return 0  -- over limit, return 0
else
  return 1  -- not over limit, return 1
end
