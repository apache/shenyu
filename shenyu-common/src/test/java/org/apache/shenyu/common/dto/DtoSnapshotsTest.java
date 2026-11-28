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

package org.apache.shenyu.common.dto;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for read-only DTO snapshots and the mutable wire format.
 */
class DtoSnapshotsTest {

    @Test
    void testNullSourcesAndReuse() throws ReflectiveOperationException {
        assertNull(DtoSnapshots.freeze((PluginData) null));
        assertNull(DtoSnapshots.freeze((SelectorData) null));
        assertNull(DtoSnapshots.freeze((RuleData) null));
        assertNull(DtoSnapshots.freeze((ConditionData) null));
        for (Object source : sources()) {
            Object snapshot = freeze(source);
            assertNotSame(source, snapshot);
            assertFalse(frozen(source));
            assertTrue(frozen(snapshot));
            assertSame(snapshot, freeze(snapshot));
        }
    }

    @Test
    void testCopiesEveryFieldAndPreservesEquality() throws ReflectiveOperationException {
        for (Object source : sources()) {
            Object snapshot = freeze(source);
            assertNonDefaultFields(source);
            assertCopiedFields(source, snapshot);
            assertEquals(source, snapshot);
            assertEquals(snapshot, source);
            assertEquals(source.hashCode(), snapshot.hashCode());
        }
    }

    @Test
    void testEveryPublicSetterRejectsWrites() throws ReflectiveOperationException {
        for (Object source : sources()) {
            Object snapshot = freeze(source);
            assertAllSettersReject(snapshot);
            assertCopiedFields(source, snapshot);
            assertEquals(source.hashCode(), snapshot.hashCode());
        }
    }

    @Test
    void testSourceMutationsDoNotChangeSnapshot() throws ReflectiveOperationException {
        for (Object source : sources()) {
            Object snapshot = freeze(source);
            Object expected = freeze(source);
            mutateConditions(source);
            invokeSetters(source, "changed");
            assertCopiedFields(expected, snapshot);
            assertEquals(expected, snapshot);
            assertEquals(expected.hashCode(), snapshot.hashCode());
            assertNotEquals(source, snapshot);
            assertFalse(frozen(source));
        }
    }

    @Test
    void testBothConditionListsAndElementsAreReadOnly() throws ReflectiveOperationException {
        for (Object source : sources()) {
            Object snapshot = freeze(source);
            for (List<ConditionData> conditions : conditionLists(snapshot)) {
                assertReadOnlyList(conditions);
                assertAllSettersReject(conditions.get(0));
                assertNull(conditions.get(1));
            }
        }
    }

    @Test
    void testNullAndEmptyConditionLists() {
        SelectorData selector = new SelectorData();
        RuleData rule = new RuleData();
        assertNull(DtoSnapshots.freeze(selector).getConditionList());
        assertNull(DtoSnapshots.freeze(selector).getBeforeConditionList());
        assertNull(DtoSnapshots.freeze(rule).getConditionDataList());
        assertNull(DtoSnapshots.freeze(rule).getBeforeConditionDataList());
        selector.setConditionList(new ArrayList<>());
        selector.setBeforeConditionList(new ArrayList<>());
        rule.setConditionDataList(new ArrayList<>());
        rule.setBeforeConditionDataList(new ArrayList<>());
        for (Object snapshot : Arrays.asList(DtoSnapshots.freeze(selector), DtoSnapshots.freeze(rule))) {
            for (List<ConditionData> conditions : conditionLists(snapshot)) {
                assertTrue(conditions.isEmpty());
                assertThrows(UnsupportedOperationException.class, () -> conditions.add(new ConditionData()));
            }
        }
    }

    @Test
    void testNullableContinuedRetainsRawValue() throws ReflectiveOperationException {
        for (Boolean continued : Arrays.asList(null, Boolean.FALSE, Boolean.TRUE)) {
            SelectorData source = SelectorData.builder().continued(continued).build();
            SelectorData snapshot = DtoSnapshots.freeze(source);
            assertCopiedFields(source, snapshot);
            assertEquals(source, snapshot);
            assertEquals(snapshot, source);
            assertEquals(source.hashCode(), snapshot.hashCode());
            assertEquals(source.getContinued(), snapshot.getContinued());
            assertEquals(GsonUtils.getInstance().toJson(source), GsonUtils.getInstance().toJson(snapshot));
        }
    }

    @Test
    void testBuildersAndBaseDataRemainMutable() throws ReflectiveOperationException {
        List<Object> values = Arrays.asList(new BaseData(), PluginData.builder().build(),
                SelectorData.builder().build(), RuleData.builder().build(), new ConditionData());
        for (Object value : values) {
            assertFalse(frozen(value));
            invokeSetters(value, "builder");
            assertNonDefaultFields(value);
        }
    }

    @Test
    void testRejectsUnknownSubclasses() {
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(new PluginData() { }));
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(new SelectorData() { }));
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(new RuleData() { }));
        ConditionData unknown = new ConditionData() { };
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(unknown));
        SelectorData selector = new SelectorData();
        RuleData rule = new RuleData();
        selector.setConditionList(Collections.singletonList(unknown));
        rule.setConditionDataList(Collections.singletonList(unknown));
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(selector));
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(rule));
        selector.setConditionList(null);
        rule.setConditionDataList(null);
        selector.setBeforeConditionList(Collections.singletonList(unknown));
        rule.setBeforeConditionDataList(Collections.singletonList(unknown));
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(selector));
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(rule));
        assertFalse(selector.frozen());
        assertFalse(rule.frozen());
        assertFalse(unknown.frozen());
        unknown.markFrozen();
        assertThrows(IllegalArgumentException.class, () -> DtoSnapshots.freeze(unknown));
    }

    @Test
    void testFrozenConditionsCanBeReused() throws ReflectiveOperationException {
        ConditionData condition = DtoSnapshots.freeze(condition("shared"));
        SelectorData selector = SelectorData.builder().conditionList(Collections.singletonList(condition))
                .beforeConditionList(Collections.singletonList(condition)).build();
        RuleData rule = RuleData.builder().conditionDataList(Collections.singletonList(condition))
                .beforeConditionDataList(Collections.singletonList(condition)).build();
        for (Object snapshot : Arrays.asList(DtoSnapshots.freeze(selector), DtoSnapshots.freeze(rule))) {
            for (List<ConditionData> conditions : conditionLists(snapshot)) {
                assertSame(condition, conditions.get(0));
                assertThrows(UnsupportedOperationException.class, () -> conditions.add(condition));
            }
        }
    }

    @Test
    void testGsonAndJacksonRoundTripsAreMutableAndOmitFrozenState() throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        for (Object source : sources()) {
            Object snapshot = freeze(source);
            String gsonJson = GsonUtils.getInstance().toJson(snapshot);
            String jacksonJson = mapper.writeValueAsString(snapshot);
            assertEquals(mapper.readTree(GsonUtils.getInstance().toJson(source)), mapper.readTree(gsonJson));
            assertEquals(mapper.readTree(mapper.writeValueAsString(source)), mapper.readTree(jacksonJson));
            assertWireFields(source, mapper.readTree(gsonJson));
            assertWireFields(source, mapper.readTree(jacksonJson));
            assertMutableRoundTrip(snapshot, GsonUtils.getInstance().fromJson(gsonJson, source.getClass()));
            assertMutableRoundTrip(snapshot, mapper.readValue(jacksonJson, source.getClass()));
        }
    }

    @Test
    void testJsonCannotSetFrozenState() throws Exception {
        // Even a permissive wire reader must not accept the internal snapshot state.
        ObjectMapper mapper = new ObjectMapper().disable(com.fasterxml.jackson.databind.DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
        for (Object source : sources()) {
            ObjectNode json = mapper.valueToTree(source);
            injectFrozenState(json);
            assertMutableRoundTrip(source, GsonUtils.getInstance().fromJson(json.toString(), source.getClass()));
            assertMutableRoundTrip(source, mapper.readValue(json.toString(), source.getClass()));
        }
        assertFalse(GsonUtils.getInstance().fromJson("{\"frozen\":true}", BaseData.class).frozen());
        assertFalse(mapper.readValue("{\"frozen\":true}", BaseData.class).frozen());
    }

    private static List<Object> sources() throws ReflectiveOperationException {
        List<Object> sources = Arrays.asList(new PluginData(), new SelectorData(), new RuleData(), new ConditionData());
        for (Object source : sources) {
            invokeSetters(source, "source");
        }
        return sources;
    }

    private static Object freeze(final Object source) throws ReflectiveOperationException {
        return DtoSnapshots.class.getMethod("freeze", source.getClass()).invoke(null, source);
    }

    private static boolean frozen(final Object value) {
        if (value instanceof BaseData) {
            return ((BaseData) value).frozen();
        }
        return ((ConditionData) value).frozen();
    }

    private static void invokeSetters(final Object value, final String prefix) throws ReflectiveOperationException {
        for (Method method : value.getClass().getMethods()) {
            if (method.getName().startsWith("set")) {
                method.invoke(value, arguments(method, prefix));
            }
        }
    }

    private static Object[] arguments(final Method method, final String prefix) throws ReflectiveOperationException {
        Class<?>[] types = method.getParameterTypes();
        Object[] arguments = new Object[types.length];
        for (int i = 0; i < types.length; i++) {
            String name = prefix + method.getName() + i;
            if (types[i] == String.class) {
                arguments[i] = name;
            } else if (types[i] == Boolean.class || types[i] == boolean.class) {
                arguments[i] = "changed".equals(prefix);
            } else if (types[i] == Integer.class || types[i] == int.class) {
                arguments[i] = "changed".equals(prefix) ? 73 : 31;
            } else if (types[i] == List.class) {
                arguments[i] = new ArrayList<>(Arrays.asList(condition(name), null));
            } else {
                throw new IllegalArgumentException("Add a test value for " + method);
            }
        }
        return arguments;
    }

    private static ConditionData condition(final String prefix) throws ReflectiveOperationException {
        ConditionData condition = new ConditionData();
        invokeSetters(condition, prefix);
        return condition;
    }

    private static void assertAllSettersReject(final Object snapshot) throws ReflectiveOperationException {
        int setters = 0;
        for (Method method : snapshot.getClass().getMethods()) {
            if (method.getName().startsWith("set")) {
                setters++;
                Object[] arguments = arguments(method, "changed");
                InvocationTargetException exception = assertThrows(InvocationTargetException.class,
                        () -> method.invoke(snapshot, arguments), method.toString());
                assertTrue(exception.getCause() instanceof UnsupportedOperationException, method.toString());
                if (Arrays.stream(method.getParameterTypes()).noneMatch(Class::isPrimitive)) {
                    InvocationTargetException nullException = assertThrows(InvocationTargetException.class,
                            () -> method.invoke(snapshot, new Object[method.getParameterCount()]), method.toString());
                    assertTrue(nullException.getCause() instanceof UnsupportedOperationException, method.toString());
                }
            }
        }
        assertTrue(setters > 0);
    }

    private static List<Field> valueFields(final Class<?> type) {
        List<Field> fields = new ArrayList<>();
        for (Class<?> current = type; current != Object.class; current = current.getSuperclass()) {
            for (Field field : current.getDeclaredFields()) {
                if (!Modifier.isStatic(field.getModifiers()) && !Modifier.isTransient(field.getModifiers())) {
                    field.setAccessible(true);
                    fields.add(field);
                }
            }
        }
        return fields;
    }

    private static void assertNonDefaultFields(final Object source) throws ReflectiveOperationException {
        Object defaults = source.getClass().getConstructor().newInstance();
        for (Field field : valueFields(source.getClass())) {
            assertNotNull(field.get(source), field.toString());
            assertNotEquals(field.get(defaults), field.get(source), field.toString());
        }
        for (List<ConditionData> conditions : conditionLists(source)) {
            for (ConditionData condition : conditions) {
                if (Objects.nonNull(condition)) {
                    assertNonDefaultFields(condition);
                }
            }
        }
    }

    private static void assertCopiedFields(final Object source, final Object snapshot) throws IllegalAccessException {
        assertEquals(source.getClass(), snapshot.getClass());
        for (Field field : valueFields(source.getClass())) {
            Object original = field.get(source);
            Object copied = field.get(snapshot);
            assertEquals(original, copied, field.toString());
            if (original instanceof List<?>) {
                assertNotSame(original, copied, field.toString());
                List<?> originals = (List<?>) original;
                List<?> copies = (List<?>) copied;
                for (int i = 0; i < originals.size(); i++) {
                    if (Objects.nonNull(originals.get(i))) {
                        assertNotSame(originals.get(i), copies.get(i));
                        assertCopiedFields(originals.get(i), copies.get(i));
                    }
                }
            }
        }
    }

    private static List<List<ConditionData>> conditionLists(final Object value) {
        if (value instanceof SelectorData) {
            SelectorData selector = (SelectorData) value;
            return Arrays.asList(selector.getConditionList(), selector.getBeforeConditionList());
        }
        if (value instanceof RuleData) {
            RuleData rule = (RuleData) value;
            return Arrays.asList(rule.getConditionDataList(), rule.getBeforeConditionDataList());
        }
        return Collections.emptyList();
    }

    private static void mutateConditions(final Object value) throws ReflectiveOperationException {
        for (List<ConditionData> conditions : conditionLists(value)) {
            for (ConditionData condition : conditions) {
                if (Objects.nonNull(condition)) {
                    assertFalse(condition.frozen());
                    invokeSetters(condition, "changed");
                }
            }
            conditions.clear();
            conditions.add(new ConditionData());
        }
    }

    private static void assertReadOnlyList(final List<ConditionData> conditions) {
        assertThrows(UnsupportedOperationException.class, () -> conditions.add(new ConditionData()));
        assertThrows(UnsupportedOperationException.class, () -> conditions.set(0, new ConditionData()));
        assertThrows(UnsupportedOperationException.class, () -> conditions.remove(0));
        assertThrows(UnsupportedOperationException.class, conditions::clear);
        assertThrows(UnsupportedOperationException.class, () -> conditions.replaceAll(condition -> null));
        assertThrows(UnsupportedOperationException.class, () -> conditions.sort((left, right) -> 0));
        assertThrows(UnsupportedOperationException.class, () -> conditions.subList(0, 1).clear());
        Iterator<ConditionData> iterator = conditions.iterator();
        iterator.next();
        assertThrows(UnsupportedOperationException.class, iterator::remove);
    }

    private static void assertWireFields(final Object value, final JsonNode json) throws IllegalAccessException {
        List<Field> fields = valueFields(value.getClass());
        assertEquals(fields.size(), json.size());
        assertFalse(json.has("frozen"));
        for (Field field : fields) {
            JsonNode node = json.get(field.getName());
            assertNotNull(node, field.toString());
            if (field.get(value) instanceof List<?>) {
                List<?> conditions = (List<?>) field.get(value);
                for (int i = 0; i < conditions.size(); i++) {
                    if (Objects.nonNull(conditions.get(i))) {
                        assertWireFields(conditions.get(i), node.get(i));
                    }
                }
            }
        }
    }

    private static void assertMutableRoundTrip(final Object expected, final Object actual) throws ReflectiveOperationException {
        assertCopiedFields(expected, actual);
        assertEquals(expected, actual);
        assertEquals(actual, expected);
        assertEquals(expected.hashCode(), actual.hashCode());
        assertFalse(frozen(actual));
        mutateConditions(actual);
        assertDoesNotThrow(() -> invokeSetters(actual, "changed"));
    }

    private static void injectFrozenState(final JsonNode node) {
        if (node.isObject()) {
            ((ObjectNode) node).put("frozen", true);
        }
        for (JsonNode child : node) {
            if (child.isContainerNode()) {
                injectFrozenState(child);
            }
        }
    }
}
