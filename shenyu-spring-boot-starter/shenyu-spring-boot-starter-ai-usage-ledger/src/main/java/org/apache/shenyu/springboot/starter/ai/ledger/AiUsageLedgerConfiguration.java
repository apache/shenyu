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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.ledger.LedgerPriceBook;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.metrics.AiCallObserver;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.http.MediaType;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerResponse;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static org.springframework.web.reactive.function.server.RequestPredicates.GET;

/** Opt-in ledger using existing observation and logging extension points, with no new accounting debit. */
@Configuration(proxyBeanMethods = false)
@ConditionalOnProperty(value = "shenyu.ai.usage-ledger.enabled", havingValue = "true")
public class AiUsageLedgerConfiguration {

    private static final String PREFIX = "shenyu.ai.usage-ledger.";

    private static final String OBSERVER = "aiUsageLedgerObserver";

    /**
     * Explicit clock, application replaceable without taking over unrelated clocks.
     * @return UTC observation clock
     */
    @Bean(name = "aiUsageLedgerClock")
    @ConditionalOnMissingBean(name = "aiUsageLedgerClock")
    public Clock ledgerClock() {
        return Clock.systemUTC();
    }

    /**
     * Bounded volatile reference sink.
     * @param environment trusted deployment configuration
     * @return finite store
     */
    @Bean
    @ConditionalOnMissingBean(UsageLedgerStore.class)
    public UsageLedgerStore ledgerStore(final Environment environment) {
        return new InMemoryUsageLedger(environment.getProperty(PREFIX + "store-capacity", Integer.class, 10000),
                Duration.parse(environment.getProperty(PREFIX + "retention", "PT24H")));
    }

    /**
     * Install operator prices only; absent prices remain unknown.
     * @param environment trusted deployment configuration
     * @return atomically replaceable price book
     */
    @Bean
    @ConditionalOnMissingBean(LedgerPriceBook.class)
    public LedgerPriceBook ledgerPrices(final Environment environment) {
        String source = environment.getProperty(PREFIX + "prices", "[]");
        if (source.length() > 65536) {
            throw new IllegalArgumentException("Price configuration is too large");
        }
        try {
            ObjectMapper json = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
                    .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS).enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS);
            JsonNode array = json.readTree(source);
            if (!array.isArray() || array.size() > 64) {
                throw new IllegalArgumentException("Invalid price array");
            }
            List<LedgerPriceBook.Price> prices = new ArrayList<>();
            Set<String> required = Set.of("version", "source", "provider", "model", "clientProtocol", "currency", "from", "inputPerMillion", "outputPerMillion");
            Set<String> allowed = Set.of("version", "source", "provider", "model", "clientProtocol", "currency", "from", "until", "inputPerMillion", "outputPerMillion");
            for (JsonNode node : array) {
                Set<String> fields = new java.util.HashSet<>();
                node.fieldNames().forEachRemaining(fields::add);
                if (!node.isObject() || !fields.containsAll(required) || !allowed.containsAll(fields)
                        || !node.path("inputPerMillion").isNumber() || !node.path("outputPerMillion").isNumber()) {
                    throw new IllegalArgumentException("Unknown or invalid price fields");
                }
                prices.add(new LedgerPriceBook.Price(text(node, "version"), text(node, "source"), text(node, "provider"),
                        text(node, "model"), text(node, "clientProtocol"), text(node, "currency"), Instant.parse(text(node, "from")),
                        !node.has("until") || node.get("until").isNull() ? null : Instant.parse(text(node, "until")),
                        node.get("inputPerMillion").decimalValue(), node.get("outputPerMillion").decimalValue()));
            }
            return new LedgerPriceBook(prices);
        } catch (Exception error) {
            throw new IllegalArgumentException("Invalid AI usage price configuration");
        }
    }

    /**
     * Map only trusted current context; absent attribution is not fabricated.
     * @param environment deployment namespace
     * @return replaceable attribution SPI
     */
    @Bean
    @ConditionalOnMissingBean(LedgerAttributionResolver.class)
    public LedgerAttributionResolver ledgerAttribution(final Environment environment) {
        return LedgerAttributionResolver.fromContext(environment.getProperty(PREFIX + "namespace"));
    }

    /**
     * Reuse normalized usage, keeping richer #7955 mappings behind an explicit SPI.
     * @return quantity mapper
     */
    @Bean
    @ConditionalOnMissingBean(LedgerUsageResolver.class)
    public LedgerUsageResolver ledgerQuantities() {
        return LedgerUsageResolver.currentContract();
    }

    /**
     * Reuse the existing bounded log collector and logging queue metrics.
     * @param environment queue capacity
     * @param store application-owned sink
     * @return owned collector, closed on context shutdown
     */
    @Bean(initMethod = "start", destroyMethod = "close")
    public UsageLedgerCollector ledgerCollector(final Environment environment, final UsageLedgerStore store) {
        return new UsageLedgerCollector(environment.getProperty(PREFIX + "queue-capacity", Integer.class, 1024), store);
    }

    /**
     * Bind the existing per-attempt and logical observation contract.
     * @param prices operator prices
     * @param collector asynchronous delivery
     * @param attribution trusted attribution
     * @param quantities shared normalized counts
     * @param clock observation clock
     * @return recorder without creating another independently selected observer bean
     */
    @Bean
    public AiUsageLedgerRecorder ledgerRecorder(final LedgerPriceBook prices, final UsageLedgerCollector collector,
                                               final LedgerAttributionResolver attribution, final LedgerUsageResolver quantities,
                                               @Qualifier("aiUsageLedgerClock") final Clock clock) {
        return new AiUsageLedgerRecorder(prices, collector, attribution, quantities, clock);
    }

    /**
     * Explicit canonical fanout preserves preexisting application sinks exactly once.
     * @param recorder ledger mapping
     * @param factory application observers, excluding this bean before instantiation
     * @return explicit composite
     */
    @Bean(name = OBSERVER)
    @ConditionalOnMissingBean(name = OBSERVER)
    public AiCallObserver ledgerObserver(final AiUsageLedgerRecorder recorder, final ConfigurableListableBeanFactory factory) {
        List<AiCallObserver> delegates = Arrays.stream(factory.getBeanNamesForType(AiCallObserver.class))
                .filter(name -> !OBSERVER.equals(name)).map(name -> factory.getBean(name, AiCallObserver.class)).toList();
        return new LedgerObserver(recorder, delegates);
    }

    /**
     * Authorized query/revaluation API.
     * @param store bounded sink
     * @param prices operator prices
     * @param clock explicit clock
     * @return metadata-only service
     */
    @Bean
    public UsageLedgerService ledgerService(final UsageLedgerStore store, final LedgerPriceBook prices,
                                            @Qualifier("aiUsageLedgerClock") final Clock clock) {
        return new UsageLedgerService(store, prices, clock);
    }

    /**
     * Optional read-only HTTP export requires an application security adapter.
     * @param service ledger queries
     * @param authorizers trusted principal-to-scope mapping
     * @return authenticated bounded route
     */
    @Bean
    @ConditionalOnProperty(value = PREFIX + "exports-enabled", havingValue = "true")
    public RouterFunction<ServerResponse> ledgerExports(final UsageLedgerService service, final ObjectProvider<LedgerExportAuthorizer> authorizers) {
        LedgerExportAuthorizer authorizer = authorizers.getIfAvailable();
        if (Objects.isNull(authorizer)) {
            throw new IllegalStateException("Ledger HTTP export requires an application authorizer");
        }
        return RouterFunctions.route(GET("/shenyu/ai/usage-ledger"), request -> {
            if (request.queryParams().keySet().stream().anyMatch(name -> !"limit".equals(name))) {
                return ServerResponse.badRequest().bodyValue(Map.of("error", "INVALID_LEDGER_QUERY"));
            }
            String value = request.queryParam("limit").orElse("100");
            Mono<UsageLedgerStore.Scope> scope = request.principal().switchIfEmpty(Mono.error(new UnauthenticatedException()))
                    .flatMap(authorizer::authorize).switchIfEmpty(Mono.error(new SecurityException()));
            return scope.flatMap(authorized -> Mono.fromCallable(() -> {
                if (value.length() > 4) {
                    throw new IllegalArgumentException("Invalid export limit");
                }
                return service.export(authorized, Integer.parseInt(value));
            }).subscribeOn(Schedulers.boundedElastic()))
                    .flatMap(result -> ServerResponse.ok().contentType(MediaType.APPLICATION_JSON).bodyValue(result))
                    .onErrorResume(UnauthenticatedException.class, error -> ServerResponse.status(401).bodyValue(Map.of("error", "AUTHENTICATION_REQUIRED")))
                    .onErrorResume(SecurityException.class, error -> ServerResponse.status(403).bodyValue(Map.of("error", "LEDGER_ACCESS_DENIED")))
                    .onErrorResume(IllegalArgumentException.class, error -> ServerResponse.badRequest().bodyValue(Map.of("error", "INVALID_LEDGER_QUERY")))
                    .onErrorResume(RuntimeException.class, error -> ServerResponse.status(503).bodyValue(Map.of("error", "LEDGER_UNAVAILABLE")));
        });
    }

    private static String text(final JsonNode node, final String field) {
        if (!node.path(field).isTextual()) {
            throw new IllegalArgumentException("Invalid price text");
        }
        return node.get(field).textValue();
    }

    private static final class UnauthenticatedException extends RuntimeException {
    }
}
