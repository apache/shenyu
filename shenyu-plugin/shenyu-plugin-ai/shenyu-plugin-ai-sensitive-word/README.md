# shenyu-plugin-ai-sensitive-word

The sensitive word plugin rejects a request whose body contains a word of the dictionary
configured for the matched rule. The dictionary lives in a **redis set**, so it can be maintained
by the operations team without redeploying shenyu.

It is aimed at the content compliance of an AI gateway, where a prompt must not reach the model
with forbidden content, but it only inspects the request body, so it also applies to plain HTTP
routes.

## How it works

- The dictionary is read with `SMEMBERS <rule.redisKey>` and compiled into an
  [Aho-Corasick](../main/java/org/apache/shenyu/plugin/ai/sensitive/word/ac/AhoCorasick.java)
  automaton, which reports **every** matching word, including nested and overlapping ones, for
  example both `中国` and `中国银行` for the text `中国银行`.
- The body is read with the shared `ServerWebExchangeUtils#rewriteRequestBody`, the whole path is
  reactive and the automaton is compiled on a bounded elastic thread, so the gateway event loop is
  never blocked.
- A compiled dictionary is reused for `refreshIntervalSeconds` and then read from redis again, so
  a dictionary update takes effect within that interval. Updating the rule in the admin console
  drops the cached dictionary immediately.
- If redis is unreachable the request is **passed through** (fail open, a warning is logged): a
  broken dictionary must not take the traffic down.

## Configuration

Plugin level (`config` of the plugin, the redis client used to read the dictionaries):

```json
{
  "url": "127.0.0.1:6379",
  "password": "",
  "database": 0,
  "mode": "standalone",
  "maxIdle": 8,
  "minIdle": 0,
  "maxActive": 8
}
```

Rule level (`handle` of the rule):

| field | type | default | description |
| --- | --- | --- | --- |
| `redisKey` | string | `shenyu:sensitive:words` | the redis set holding the dictionary of this rule |
| `refreshIntervalSeconds` | long | `300` | how long a compiled dictionary is reused, `0` reads it on every request |

## Dictionary format

A redis set of words, one word per member, for example:

```
SADD shenyu:sensitive:words "bad word 1"
SADD shenyu:sensitive:words "bad word 2"
```

The word list itself is **not** part of this repository: every deployment is expected to provide
its own dictionary, because the content of such a list depends on the country, the business and
the compliance rules that apply to it.
