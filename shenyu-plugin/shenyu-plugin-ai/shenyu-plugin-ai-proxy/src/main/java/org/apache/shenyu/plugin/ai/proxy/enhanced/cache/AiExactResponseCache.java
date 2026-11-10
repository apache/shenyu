/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Ticker;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamRequest;
import org.apache.shenyu.plugin.ai.proxy.enhanced.model.AiUpstreamResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.LongAdder;
import java.util.function.Function;

/**
 * Local, bounded exact cache for explicitly authorized native text Chat requests.
 * It never subscribes to a body itself or turns a failed consumer into another attempt.
 */
public final class AiExactResponseCache {

    private static final ObjectMapper MAPPER = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final Set<String> REQUEST_FIELDS = Set.of("messages", "model", "stream", "max_tokens", "max_completion_tokens", "temperature", "top_p",
            "presence_penalty", "frequency_penalty", "stop", "seed", "n", "logit_bias");

    private static final Set<String> PROVIDERS = Set.of("openai", "openaicompatible", "deepseek", "aliyun", "moonshot");

    private static final Set<String> ROLES = Set.of("system", "user", "assistant");

    private static final Set<String> UNSUPPORTED_HEADERS = Set.of("set-cookie", "set-cookie2", "vary", "content-encoding", "content-range", "www-authenticate", "proxy-authenticate");

    private static final Set<String> REPLAY_HEADERS = Set.of("content-type", "content-language");

    private static final int MAX_DEPTH = 32;

    private static final int ENTRY_OVERHEAD = 256;

    private final AiResponseCacheProperties properties;

    private final Cache<String, Entry> cache;

    private final AtomicInteger fills = new AtomicInteger();

    private final AtomicLong generation = new AtomicLong();

    private final LongAdder hits = new LongAdder();

    private final LongAdder misses = new LongAdder();

    private final LongAdder bypasses = new LongAdder();

    private final LongAdder writes = new LongAdder();

    /**
     * Create a cache using the system monotonic clock.
     *
     * @param properties bounded cache configuration
     */
    public AiExactResponseCache(final AiResponseCacheProperties properties) {
        this(properties, Ticker.systemTicker());
    }

    AiExactResponseCache(final AiResponseCacheProperties properties, final Ticker ticker) {
        this.properties = Objects.requireNonNull(properties);
        cache = Caffeine.newBuilder().maximumWeight(properties.getMaxWeightBytes())
                .weigher((final String key, final Entry entry) -> entry.weight())
                .expireAfterWrite(Duration.ofSeconds(properties.getTtlSeconds())).ticker(ticker).build();
    }

    /**
     * Execute against the cache after authentication, routing and request preparation.
     *
     * @param originalBody original client JSON
     * @param prepared actual upstream request
     * @param provider native provider identifier, independently verified by the engine
     * @param clientProtocol client protocol
     * @param targetProtocol actual upstream protocol
     * @param stream whether streaming was requested
     * @param hasFallback whether the request may fall back
     * @param consumer downstream response writer
     * @param upstream original transport operation accepting a response writer
     * @return lazy response completion
     */
    public Mono<Void> execute(final String originalBody, final AiUpstreamRequest prepared, final String provider,
                              final String clientProtocol, final String targetProtocol, final boolean stream, final boolean hasFallback,
                              final Function<AiUpstreamResponse, Mono<Void>> consumer,
                              final Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> upstream) {
        return Mono.deferContextual(context -> {
            final long epoch = generation.get();
            Object scope = context.getOrDefault(AiResponseCacheScope.REACTOR_KEY, null);
            if (!properties.isEnabled() || stream || hasFallback || !(scope instanceof AiResponseCacheScope)
                    || !"openai-chat".equals(clientProtocol) || !"openai-chat".equals(targetProtocol) || !knownProvider(provider)) {
                return bypass(consumer, upstream);
            }
            String key;
            Entry entry;
            try {
                key = key(originalBody, prepared, (AiResponseCacheScope) scope, provider, clientProtocol, targetProtocol);
                if (Objects.isNull(key)) {
                    return bypass(consumer, upstream);
                }
                entry = cache.getIfPresent(key);
            } catch (Exception ignored) {
                return bypass(consumer, upstream);
            }
            if (Objects.nonNull(entry)) {
                hits.increment();
                return Mono.defer(() -> consumer.apply(entry.replay()));
            }
            misses.increment();
            if (!acquire()) {
                return bypass(consumer, upstream);
            }
            Fill fill = new Fill(key, epoch);
            return Mono.defer(() -> upstream.apply(response -> fill.consume(response, consumer)))
                    .doOnSuccess(ignored -> fill.publish()).doFinally(ignored -> {
                        fill.clear();
                        fills.decrementAndGet();
                    });
        });
    }

    /**
     * Invalidate entries and prevent active fills from republishing an older generation.
     */
    public synchronized void invalidateAll() {
        generation.incrementAndGet();
        cache.invalidateAll();
    }

    /**
     * Get aggregate counters, without request identity or provider usage accounting.
     *
     * @return current non-transactional counter snapshot
     */
    public Snapshot stats() {
        return new Snapshot(hits.sum(), misses.sum(), bypasses.sum(), writes.sum());
    }

    private Mono<Void> bypass(final Function<AiUpstreamResponse, Mono<Void>> consumer,
                              final Function<Function<AiUpstreamResponse, Mono<Void>>, Mono<Void>> upstream) {
        bypasses.increment();
        return Mono.defer(() -> upstream.apply(consumer));
    }

    private boolean acquire() {
        int current = fills.get();
        while (current < properties.getMaxConcurrentFills()) {
            if (fills.compareAndSet(current, current + 1)) {
                return true;
            }
            current = fills.get();
        }
        return false;
    }

    private boolean knownProvider(final String provider) {
        return Objects.nonNull(provider) && PROVIDERS.contains(provider.trim().toLowerCase(Locale.ROOT).replace("-", "").replace("_", ""));
    }

    private String key(final String original, final AiUpstreamRequest request, final AiResponseCacheScope scope,
                       final String provider, final String clientProtocol, final String targetProtocol) throws IOException, NoSuchAlgorithmException {
        String[] isolation = {scope.tenant(), scope.subject(), scope.accessScope(), scope.policyRevision(), scope.promptVersion(), scope.selectorId(), scope.ruleId()};
        ArrayNode key = MAPPER.createArrayNode();
        for (String value : isolation) {
            if (Objects.isNull(value) || value.isBlank() || value.length() > 256) {
                return null;
            }
            key.add(value);
        }
        if (Objects.isNull(original) || original.length() > properties.getMaxRequestBytes() || Objects.isNull(request)
                || Objects.isNull(request.getBody()) || request.getBody().length > properties.getMaxRequestBytes()
                || Objects.isNull(request.getUri()) || !"POST".equalsIgnoreCase(request.getMethod())) {
            return null;
        }
        byte[] originalBytes = original.getBytes(StandardCharsets.UTF_8);
        if (originalBytes.length > properties.getMaxRequestBytes()) {
            return null;
        }
        JsonNode originalJson = parse(originalBytes);
        JsonNode effectiveJson = parse(request.getBody());
        if (!textRequest(originalJson, false) || !textRequest(effectiveJson, true)) {
            return null;
        }
        Map<String, List<String>> headers = normalizedHeaders(request.getHeaders());
        if (headerWeight(headers) > properties.getMaxRequestBytes()) {
            return null;
        }
        key.add(canonical(originalJson)).add(canonical(effectiveJson)).add(provider).add(clientProtocol).add(targetProtocol)
                .add(request.getUri().toString()).add(request.getMethod()).add(MAPPER.valueToTree(headers));
        // Only this digest enters the store: prompts, credentials and identity are not retained as keys.
        return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(MAPPER.writeValueAsBytes(key)));
    }

    private JsonNode parse(final byte[] bytes) throws IOException {
        // Bound nesting before constructing a tree or recursively canonicalizing it.
        try (JsonParser parser = MAPPER.createParser(bytes)) {
            int depth = 0;
            JsonToken token;
            while (Objects.nonNull(token = parser.nextToken())) {
                if ((token == JsonToken.START_OBJECT || token == JsonToken.START_ARRAY) && ++depth > MAX_DEPTH) {
                    throw new IOException("Cache JSON nesting limit exceeded");
                }
                if (token == JsonToken.END_OBJECT || token == JsonToken.END_ARRAY) {
                    depth--;
                }
            }
        }
        return MAPPER.readTree(bytes);
    }

    private JsonNode canonical(final JsonNode node) {
        if (node.isObject()) {
            ObjectNode result = MAPPER.createObjectNode();
            Map<String, JsonNode> sorted = new TreeMap<>();
            node.fields().forEachRemaining(field -> sorted.put(field.getKey(), field.getValue()));
            sorted.forEach((name, value) -> result.set(name, canonical(value)));
            return result;
        }
        if (node.isArray()) {
            ArrayNode result = MAPPER.createArrayNode();
            node.forEach(value -> result.add(canonical(value)));
            return result;
        }
        return node;
    }

    private boolean textRequest(final JsonNode node, final boolean requireModel) {
        if (Objects.isNull(node) || !node.isObject()) {
            return false;
        }
        List<String> names = new ArrayList<>();
        node.fieldNames().forEachRemaining(names::add);
        if (!REQUEST_FIELDS.containsAll(names)
                || (requireModel || node.has("model")) && (!node.path("model").isTextual() || node.path("model").asText().isBlank())
                || node.has("stream") && (!node.get("stream").isBoolean() || node.get("stream").booleanValue())) {
            return false;
        }
        JsonNode messages = node.path("messages");
        if (!messages.isArray() || messages.isEmpty()) {
            return false;
        }
        for (JsonNode message : messages) {
            if (!message.isObject() || message.size() != 2 || !message.path("role").isTextual()
                    || !ROLES.contains(message.path("role").asText()) || !message.path("content").isTextual()) {
                return false;
            }
        }
        return validParameters(node);
    }

    private boolean validParameters(final JsonNode node) {
        for (String name : List.of("temperature", "top_p", "presence_penalty", "frequency_penalty")) {
            if (node.has(name) && !node.get(name).isNumber()) {
                return false;
            }
        }
        for (String name : List.of("max_tokens", "max_completion_tokens", "seed", "n")) {
            if (node.has(name) && !node.get(name).isIntegralNumber()) {
                return false;
            }
        }
        if (node.has("stop") && !node.get("stop").isTextual()) {
            JsonNode stop = node.get("stop");
            if (!stop.isArray()) {
                return false;
            }
            for (JsonNode value : stop) {
                if (!value.isTextual()) {
                    return false;
                }
            }
        }
        if (node.has("logit_bias")) {
            JsonNode bias = node.get("logit_bias");
            if (!bias.isObject()) {
                return false;
            }
            for (JsonNode value : bias) {
                if (!value.isNumber()) {
                    return false;
                }
            }
        }
        return true;
    }

    private Map<String, List<String>> normalizedHeaders(final Map<String, List<String>> headers) {
        Map<String, List<String>> result = new TreeMap<>();
        headers.forEach((name, values) -> result.computeIfAbsent(name.toLowerCase(Locale.ROOT), ignored -> new ArrayList<>()).addAll(values));
        return result;
    }

    private long headerWeight(final Map<String, List<String>> headers) {
        long weight = 0;
        for (Map.Entry<String, List<String>> header : headers.entrySet()) {
            weight += 64L + 2L * header.getKey().length();
            for (String value : header.getValue()) {
                weight += 40L + 2L * value.length();
            }
        }
        return weight;
    }

    private Map<String, List<String>> replayHeaders(final AiUpstreamResponse response) {
        if (response.getStatusCode() != 200) {
            return null;
        }
        Map<String, List<String>> headers = normalizedHeaders(response.getHeaders());
        if (headerWeight(headers) > properties.getMaxEntryBytes() || headers.keySet().stream().anyMatch(UNSUPPORTED_HEADERS::contains)
                || !String.join(",", headers.getOrDefault("content-type", List.of())).split(";", 2)[0].trim().equalsIgnoreCase("application/json")
                || forbiddenDirective(headers, "cache-control") || forbiddenDirective(headers, "pragma")) {
            return null;
        }
        Set<String> connection = new java.util.HashSet<>();
        headers.getOrDefault("connection", List.of()).forEach(value -> {
            for (String token : value.split(",")) {
                connection.add(token.trim().toLowerCase(Locale.ROOT));
            }
        });
        if (connection.contains("content-type")) {
            return null;
        }
        Map<String, List<String>> result = new TreeMap<>();
        headers.forEach((name, values) -> {
            if (REPLAY_HEADERS.contains(name) && !connection.contains(name)) {
                result.put(name, List.copyOf(values));
            }
        });
        return Map.copyOf(result);
    }

    private boolean forbiddenDirective(final Map<String, List<String>> headers, final String name) {
        for (String value : headers.getOrDefault(name, List.of())) {
            for (String directive : value.split(",")) {
                String token = directive.trim().split("=", 2)[0].trim().toLowerCase(Locale.ROOT);
                if (Set.of("no-store", "no-cache", "private").contains(token)) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean textResponse(final byte[] body) throws IOException {
        JsonNode root = parse(body);
        if (Objects.isNull(root) || !root.isObject() || root.has("error") || !root.path("choices").isArray() || root.path("choices").isEmpty()) {
            return false;
        }
        for (JsonNode choice : root.get("choices")) {
            JsonNode message = choice.path("message");
            List<String> fields = new ArrayList<>();
            message.fieldNames().forEachRemaining(fields::add);
            if (!"stop".equals(choice.path("finish_reason").asText()) || !message.isObject() || !message.path("content").isTextual()
                    || !"assistant".equals(message.path("role").asText()) || !Set.of("role", "content", "refusal").containsAll(fields)
                    || message.has("refusal") && !message.get("refusal").isNull()) {
                return false;
            }
        }
        return true;
    }

    /**
     * Fixed-dimension local cache counters.
     *
     * @param hits requests replayed from cache
     * @param misses eligible lookups without an entry
     * @param bypasses requests forwarded without a fill, including capacity bypasses
     * @param writes completed responses inserted into the cache
     */
    public record Snapshot(long hits, long misses, long bypasses, long writes) {
    }

    private record Entry(Map<String, List<String>> headers, byte[] body, int weight) {

        private AiUpstreamResponse replay() {
            return new AiUpstreamResponse(200, headers, Flux.defer(() -> Flux.just(body.clone())));
        }
    }

    private final class Fill {

        private final String key;

        private final long epoch;

        private Candidate candidate;

        private Fill(final String key, final long epoch) {
            this.key = key;
            this.epoch = epoch;
        }

        private Mono<Void> consume(final AiUpstreamResponse response, final Function<AiUpstreamResponse, Mono<Void>> consumer) {
            return Mono.defer(() -> {
                Candidate next = null;
                try {
                    Map<String, List<String>> headers = replayHeaders(response);
                    long overhead = Objects.isNull(headers) ? Long.MAX_VALUE : ENTRY_OVERHEAD + 2L * key.length() + headerWeight(headers);
                    if (overhead < properties.getMaxEntryBytes() && overhead < properties.getMaxWeightBytes()) {
                        next = new Candidate(headers, (int) overhead);
                    }
                } catch (Exception ignored) {
                    // Cache bookkeeping must never fail the proxy exchange.
                }
                clear();
                candidate = next;
                if (Objects.isNull(next)) {
                    return consumer.apply(response);
                }
                Candidate active = next;
                AiUpstreamResponse tapped = new AiUpstreamResponse(response.getStatusCode(), response.getHeaders(), Flux.defer(() -> {
                    if (!active.subscribed.compareAndSet(false, true)) {
                        active.clear();
                    }
                    return response.getBody().doOnNext(active::accept).doOnComplete(active::complete)
                            .doOnError(ignored -> active.clear()).doOnCancel(active::clear);
                }));
                return consumer.apply(tapped).doOnSuccess(ignored -> active.consumed());
            });
        }

        private void publish() {
            try {
                Candidate active = candidate;
                Entry entry = Objects.isNull(active) ? null : active.entry();
                // Serialize with explicit invalidation to avoid an old fill repopulating the store.
                synchronized (AiExactResponseCache.this) {
                    if (Objects.nonNull(entry) && epoch == generation.get()) {
                        cache.put(key, entry);
                        writes.increment();
                    }
                }
            } catch (Exception ignored) {
                // A cache failure cannot change successful delivery or retry the upstream.
            }
        }

        private void clear() {
            if (Objects.nonNull(candidate)) {
                candidate.clear();
            }
        }
    }

    private final class Candidate {

        private final Map<String, List<String>> headers;

        private final int overhead;

        private final AtomicBoolean subscribed = new AtomicBoolean();

        private ByteArrayOutputStream bytes = new ByteArrayOutputStream();

        private boolean complete;

        private boolean consumed;

        private Candidate(final Map<String, List<String>> headers, final int overhead) {
            this.headers = headers;
            this.overhead = overhead;
        }

        private synchronized void accept(final byte[] chunk) {
            if (Objects.isNull(bytes)) {
                return;
            }
            try {
                long size = (long) overhead + bytes.size() + chunk.length;
                if (size > properties.getMaxEntryBytes() || size > properties.getMaxWeightBytes() || size > Integer.MAX_VALUE) {
                    clear();
                } else {
                    bytes.writeBytes(chunk);
                }
            } catch (Exception ignored) {
                clear();
            }
        }

        private synchronized void complete() {
            complete = true;
        }

        private synchronized void consumed() {
            consumed = true;
        }

        private synchronized Entry entry() throws IOException {
            if (Objects.isNull(bytes) || !complete || !consumed) {
                return null;
            }
            byte[] body = bytes.toByteArray();
            return textResponse(body) ? new Entry(headers, body, overhead + body.length) : null;
        }

        private synchronized void clear() {
            bytes = null;
        }
    }
}
