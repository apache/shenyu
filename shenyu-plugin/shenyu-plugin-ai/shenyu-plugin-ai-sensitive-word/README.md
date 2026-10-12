# shenyu-plugin-ai-sensitive-word

The sensitive word plugin rejects a request whose body contains a word of the dictionary configured
for the matched rule. A dictionary is the union of two sources, so a small list can be kept next to
the rule and a large one outside of shenyu:

- the `words` of the rule, separated by commas or by new lines;
- a **redis set**, read with `SMEMBERS <rule.redisKey>`, which the operations team maintains without
  redeploying shenyu.

It is aimed at the content compliance of an AI gateway, where a prompt must not reach the model with
forbidden content, but it only inspects the request body, so it also applies to plain HTTP routes.

## How it works

- The dictionary is the union of the words of the rule and of the redis set, compiled into an
  [Aho-Corasick](../main/java/org/apache/shenyu/plugin/ai/sensitive/word/ac/AhoCorasick.java)
  automaton, which reports **every** matching word, including nested and overlapping ones, for
  example both `中国` and `中国银行` for the text `中国银行`.
- The body is read with the shared `ServerWebExchangeUtils#rewriteRequestBody`, the whole path is
  reactive and the automaton is compiled on a bounded elastic thread, so the gateway event loop is
  never blocked.
- A compiled dictionary is reused for `refreshIntervalSeconds` and then read from redis again, so a
  dictionary update takes effect within that interval. Updating the rule in the admin console drops
  the cached dictionary immediately. Dictionaries are cached per `redisKey` **and** per `words`, so
  two rules never share an automaton.
- A body larger than `maxBodySize` is not buffered: the declared `Content-Length` is checked before
  the body is read, and a body whose size is not declared is checked once it has been read. Such a
  request is passed through unscanned, or rejected when the rule is `failClosed`.
- If redis is unreachable the request is **passed through** by default (fail open, a warning is
  logged): a broken dictionary must not take the traffic down. A rule with `failClosed` rejects the
  request instead. The words configured on the rule do not depend on redis, so they are enforced in
  every case.

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

Rule level (`handle` of the rule, rendered by the console rule form):

| field | type | default | description |
| --- | --- | --- | --- |
| `redisKey` | string | `shenyu:sensitive:words` | the redis set holding the dictionary of this rule |
| `refreshIntervalSeconds` | long | `300` | how long a compiled dictionary is reused, `0` reads it on every request |
| `failClosed` | boolean | `false` | reject the request when the dictionary is unavailable instead of passing it through |
| `words` | string | empty | the words of the rule, separated by commas or by new lines, merged with the redis set |
| `maxBodySize` | long | `0` | the largest body that is scanned, in bytes, `0` scans every body |

The rule `words` are convenient for a handful of words. A larger list belongs in redis: the console
renders `words` as a single line text field.

## Dictionary format

A redis set of words, one word per member, for example:

```
SADD shenyu:sensitive:words "bad word 1"
SADD shenyu:sensitive:words "bad word 2"
```

A file with one `SADD <key> "<word>"` per line can be piped into `redis-cli` directly, and the
default key can be changed with the `redisKey` of the rule.

The word list itself is **not** part of this repository: every deployment is expected to provide its
own dictionary, because the content of such a list depends on the country, the business and the
compliance rules that apply to it.

## Registration

The plugin, its rule fields and its console menu are registered by the sql scripts: `db/init/**` for
a new installation and `db/upgrade/2.7.1-upgrade-2.7.2-*` for an existing one. The plugin id is `67`
and its `plugin_handle` rows define the rule form above.
