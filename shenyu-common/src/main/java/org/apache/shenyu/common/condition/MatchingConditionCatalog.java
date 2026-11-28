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

package org.apache.shenyu.common.condition;

import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Authoritative immutable catalog of built-in matching-condition metadata.
 */
public final class MatchingConditionCatalog {

    /**
     * Current catalog schema and content version.
     */
    public static final String VERSION = "1";

    private static final List<MatchingConditionOperator> OPERATORS = List.copyOf(OperatorEnum.acquireSupport().stream()
            .map(MatchingConditionCatalog::operatorDescriptor).collect(Collectors.toList()));

    private static final List<MatchingConditionSource> SOURCES = List.copyOf(ParamTypeEnum.acquireSupport().stream()
            .map(MatchingConditionCatalog::sourceDescriptor).collect(Collectors.toList()));

    private static final Map<String, MatchingConditionSource> SOURCES_BY_ID = index(SOURCES, MatchingConditionSource::id);

    private static final Map<String, MatchingConditionOperator> OPERATORS_BY_ID = index(OPERATORS, MatchingConditionOperator::id);

    private static final Map<String, List<MatchingConditionOperator>> OPERATORS_BY_SOURCE = operatorsBySource();

    private MatchingConditionCatalog() {
    }

    /**
     * Returns the current catalog version.
     *
     * @return catalog version
     */
    public static String version() {
        return VERSION;
    }

    /**
     * Returns source descriptors in {@link ParamTypeEnum} declaration order.
     *
     * @return immutable source descriptors
     */
    public static List<MatchingConditionSource> sources() {
        return SOURCES;
    }

    /**
     * Returns operator descriptors in {@link OperatorEnum} declaration order.
     *
     * @return immutable operator descriptors
     */
    public static List<MatchingConditionOperator> operators() {
        return OPERATORS;
    }

    /**
     * Finds a source descriptor.
     *
     * @param id source identifier
     * @return source descriptor when known
     */
    public static Optional<MatchingConditionSource> source(final String id) {
        return Optional.ofNullable(SOURCES_BY_ID.get(id));
    }

    /**
     * Finds an operator descriptor.
     *
     * @param id operator identifier
     * @return operator descriptor when known
     */
    public static Optional<MatchingConditionOperator> operator(final String id) {
        return Optional.ofNullable(OPERATORS_BY_ID.get(id));
    }

    /**
     * Returns applicable operators for a source.
     *
     * @param sourceId source identifier
     * @return immutable applicable operators, or an empty list for an unknown source
     */
    public static List<MatchingConditionOperator> operatorsFor(final String sourceId) {
        return OPERATORS_BY_SOURCE.getOrDefault(sourceId, List.of());
    }

    private static MatchingConditionSource sourceDescriptor(final ParamTypeEnum source) {
        return switch (source) {
            case PATH -> unavailablePathSource(source);
            case FORM_DATA -> unavailableFormDataSource(source);
            case POST -> postSource(source);
            case URI -> uriSource(source);
            case QUERY -> namedTextSource(source, "4", "version", "v1");
            case HOST -> clientHostSource(source);
            case IP -> clientIpSource(source);
            case HEADER -> namedTextSource(source, "7", "X-Tenant", "demo");
            case COOKIE -> namedTextSource(source, "8", "session", "active");
            case REQUEST_METHOD -> requestMethodSource(source);
            case DOMAIN -> domainSource(source);
        };
    }

    private static MatchingConditionOperator operatorDescriptor(final OperatorEnum operator) {
        return switch (operator) {
            case MATCH -> createOperator(operator, "match", MatchingConditionStatus.ADVANCED, true, MatchingConditionValueFormat.PATH_ANT);
            case EQ -> createOperator(operator, "eq", MatchingConditionStatus.SUPPORTED, true, MatchingConditionValueFormat.TEXT);
            case REGEX -> createOperator(operator, "regex", MatchingConditionStatus.SUPPORTED, true, MatchingConditionValueFormat.REGEX);
            case GT, LT -> throw new IllegalArgumentException("Unsupported operator cannot be cataloged: " + operator);
            case CONTAINS -> createOperator(operator, "contains", MatchingConditionStatus.SUPPORTED, true, MatchingConditionValueFormat.TEXT);
            case STARTS_WITH -> createOperator(operator, "startsWith", MatchingConditionStatus.SUPPORTED, true, MatchingConditionValueFormat.TEXT);
            case ENDS_WITH -> createOperator(operator, "endsWith", MatchingConditionStatus.SUPPORTED, true, MatchingConditionValueFormat.TEXT);
            case TIME_BEFORE -> createOperator(operator, "TimeBefore", MatchingConditionStatus.ADVANCED, true, MatchingConditionValueFormat.DATE_TIME);
            case EXCLUDE -> createOperator(operator, "exclude", MatchingConditionStatus.ADVANCED, true, MatchingConditionValueFormat.TEXT);
            case TIME_AFTER -> createOperator(operator, "TimeAfter", MatchingConditionStatus.ADVANCED, true, MatchingConditionValueFormat.DATE_TIME);
            case PATH_PATTERN -> createOperator(operator, "pathPattern", MatchingConditionStatus.ADVANCED, true, MatchingConditionValueFormat.PATH_SPRING);
            case IS_BLANK -> createOperator(operator, "isBlank", MatchingConditionStatus.SUPPORTED, false, MatchingConditionValueFormat.NONE);
        };
    }

    private static MatchingConditionSource unavailablePathSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.DECLARED_NOT_REGISTERED, MatchingConditionValueType.PATH,
                new SourceConfiguration(false, false, uriOperatorIds(), List.of()));
    }

    private static MatchingConditionSource unavailableFormDataSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.DECLARED_NOT_REGISTERED, MatchingConditionValueType.TEXT,
                new SourceConfiguration(false, false, textOperatorIds(), List.of()));
    }

    private static MatchingConditionSource postSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.LEGACY, MatchingConditionValueType.CONTEXT_FIELD,
                new SourceConfiguration(true, true, postOperatorIds(),
                        examples(source, OperatorEnum.EQ, "3", "appKey", "demo")));
    }

    private static MatchingConditionSource uriSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.SUPPORTED, MatchingConditionValueType.PATH,
                new SourceConfiguration(false, true, uriOperatorIds(),
                        examples(source, OperatorEnum.MATCH, "1", "", "/api/**")));
    }

    private static MatchingConditionSource namedTextSource(final ParamTypeEnum source, final String exampleNumber,
                                                           final String paramName, final String paramValue) {
        return createSource(source, MatchingConditionStatus.SUPPORTED, MatchingConditionValueType.TEXT,
                new SourceConfiguration(true, true, textOperatorIds(),
                        examples(source, OperatorEnum.EQ, exampleNumber, paramName, paramValue)));
    }

    private static MatchingConditionSource clientHostSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.SUPPORTED, MatchingConditionValueType.HOST,
                new SourceConfiguration(false, true, textOperatorIds(),
                        examples(source, OperatorEnum.CONTAINS, "5", "", "client.example")));
    }

    private static MatchingConditionSource clientIpSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.SUPPORTED, MatchingConditionValueType.IP,
                new SourceConfiguration(false, true, textOperatorIds(),
                        examples(source, OperatorEnum.STARTS_WITH, "6", "", "192.168.")));
    }

    private static MatchingConditionSource requestMethodSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.SUPPORTED, MatchingConditionValueType.HTTP_METHOD,
                new SourceConfiguration(false, true, textOperatorIds(),
                        examples(source, OperatorEnum.EQ, "9", "", "GET")));
    }

    private static MatchingConditionSource domainSource(final ParamTypeEnum source) {
        return createSource(source, MatchingConditionStatus.SUPPORTED, MatchingConditionValueType.HOST,
                new SourceConfiguration(false, true, textOperatorIds(),
                        examples(source, OperatorEnum.EQ, "10", "", "api.example.com")));
    }

    private static MatchingConditionSource createSource(final ParamTypeEnum source, final MatchingConditionStatus status,
                                                        final MatchingConditionValueType valueType,
                                                        final SourceConfiguration configuration) {
        String id = source.getName();
        return new MatchingConditionSource(id, sourceKey(id, "display"), sourceKey(id, "help"), status,
                valueType, configuration.paramNameRequired(), configuration.runtimeRegistered(),
                configuration.operatorIds(), configuration.examples());
    }

    private static MatchingConditionOperator createOperator(final OperatorEnum operator, final String key,
                                                             final MatchingConditionStatus status, final boolean requiresValue,
                                                             final MatchingConditionValueFormat valueFormat) {
        return new MatchingConditionOperator(operator.getAlias(), operatorKey(key, "display"), operatorKey(key, "help"),
                status, requiresValue, valueFormat);
    }

    private static List<String> uriOperatorIds() {
        return aliases(OperatorEnum.EQ, OperatorEnum.MATCH, OperatorEnum.REGEX, OperatorEnum.CONTAINS,
                OperatorEnum.STARTS_WITH, OperatorEnum.ENDS_WITH, OperatorEnum.EXCLUDE,
                OperatorEnum.PATH_PATTERN, OperatorEnum.IS_BLANK);
    }

    private static List<String> textOperatorIds() {
        return aliases(OperatorEnum.EQ, OperatorEnum.REGEX, OperatorEnum.CONTAINS, OperatorEnum.STARTS_WITH,
                OperatorEnum.ENDS_WITH, OperatorEnum.MATCH, OperatorEnum.EXCLUDE, OperatorEnum.IS_BLANK);
    }

    private static List<String> postOperatorIds() {
        return aliases(OperatorEnum.EQ, OperatorEnum.REGEX, OperatorEnum.CONTAINS, OperatorEnum.STARTS_WITH,
                OperatorEnum.ENDS_WITH, OperatorEnum.MATCH, OperatorEnum.EXCLUDE, OperatorEnum.IS_BLANK,
                OperatorEnum.TIME_BEFORE, OperatorEnum.TIME_AFTER);
    }

    private static List<String> aliases(final OperatorEnum... operators) {
        return List.copyOf(List.of(operators).stream().map(OperatorEnum::getAlias).collect(Collectors.toList()));
    }

    private static List<MatchingConditionExample> examples(final ParamTypeEnum source, final OperatorEnum operator,
                                                           final String number, final String paramName, final String paramValue) {
        return List.of(new MatchingConditionExample(source.getName(), operator.getAlias(), paramName, paramValue,
                "matching.condition.example." + number));
    }

    private static String sourceKey(final String id, final String suffix) {
        return "matching.condition.source." + id + "." + suffix;
    }

    private static String operatorKey(final String key, final String suffix) {
        return "matching.condition.operator." + key + "." + suffix;
    }

    private static <T> Map<String, T> index(final List<T> values, final Function<T, String> idExtractor) {
        Map<String, T> result = new LinkedHashMap<>();
        values.forEach(value -> result.put(idExtractor.apply(value), value));
        return Collections.unmodifiableMap(result);
    }

    private static Map<String, List<MatchingConditionOperator>> operatorsBySource() {
        Map<String, List<MatchingConditionOperator>> result = new LinkedHashMap<>();
        SOURCES.forEach(source -> result.put(source.id(), List.copyOf(source.operatorIds().stream()
                .map(OPERATORS_BY_ID::get).collect(Collectors.toList()))));
        return Collections.unmodifiableMap(result);
    }

    private record SourceConfiguration(boolean paramNameRequired, boolean runtimeRegistered,
                                       List<String> operatorIds, List<MatchingConditionExample> examples) {
    }
}
