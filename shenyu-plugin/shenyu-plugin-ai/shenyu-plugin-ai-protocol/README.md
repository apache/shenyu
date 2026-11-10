# shenyu-plugin-ai-protocol

Shared AI protocol detection for the AI plugins. It resolves a request to a protocol
identifier (`openai-chat`, `openai-responses`) from three levels of evidence: exact or
suffix request path match, exact header hints, and — only when the stronger levels are
inconclusive — the request body shape. A level must yield exactly one candidate;
multiple candidates resolve to unknown instead of a silent order based choice.

`AiProtocolDetector#detect` and `#resolve` each expose a raw-body overload and an
already parsed payload overload; both perform the same three-level detection, so a
caller that parsed the body can reuse it without a second parse. There is no body-only
detection entry.

`AiProtocolDetector#resolve` caches the decision in the `Constants.AI_PROTOCOL` exchange
attribute: the first AI plugin of a request resolves the protocol (caching an empty
string when it stays unknown) and every later plugin reuses the same conclusion without
re-detecting.
