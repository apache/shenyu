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

package org.apache.shenyu.k8s.common;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Map;

/**
 * Kubernetes Namespace selector semantics and fail-closed validation.
 */
public final class NamespaceSelectorTest {

    @Test
    public void testMissingSelectorOrNamespaceDoesNotMatch() {
        Assertions.assertFalse(NamespaceSelector.matches(null, Map.of()));
        Assertions.assertFalse(NamespaceSelector.matches(selector("{}"), null));
        Assertions.assertFalse(NamespaceSelector.matches(selector("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"NotIn\",\"values\":[\"red\"]}]}"), null));
    }

    @Test
    public void testEmptySelectorAndOptionalCollectionsMatchPresentNamespace() {
        for (String json : new String[]{"{}", "{\"matchLabels\":{}}", "{\"matchExpressions\":[]}", "{\"matchLabels\":{},\"matchExpressions\":[]}"}) {
            Assertions.assertTrue(NamespaceSelector.matches(selector(json), Map.of()), json);
            Assertions.assertTrue(NamespaceSelector.matches(selector(json), Map.of("team", "blue")), json);
        }
    }

    @Test
    public void testMatchLabelsRequiresEveryKeyAndExactValue() {
        JsonObject selector = selector("{\"matchLabels\":{\"team\":\"blue\",\"stage\":\"production\"}}");
        Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of("team", "blue", "stage", "production", "extra", "allowed")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "blue")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "blue", "stage", "Production")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of()));
    }

    @Test
    public void testInMatchesOnlyPresentKeysWithOneOfTheValues() {
        JsonObject selector = expression("team", "In", "red", "blue");
        Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of("team", "blue")));
        Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of("team", "red")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "green")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of()));
    }

    @Test
    public void testNotInIncludesAbsentKeys() {
        JsonObject selector = expression("team", "NotIn", "red", "blue");
        Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of()));
        Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of("team", "green")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "red")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "blue")));
    }

    @Test
    public void testExistsAllowsOmittedOrEmptyValuesAndEmptyLabelValue() {
        JsonObject omitted = selector("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"Exists\"}]}");
        for (JsonObject selector : new JsonObject[]{omitted, expression("team", "Exists")}) {
            Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of("team", "blue")));
            Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of("team", "")));
            Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of()));
        }
    }

    @Test
    public void testDoesNotExistAllowsOmittedOrEmptyValues() {
        JsonObject omitted = selector("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"DoesNotExist\"}]}");
        for (JsonObject selector : new JsonObject[]{omitted, expression("team", "DoesNotExist")}) {
            Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of()));
            Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "blue")));
            Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "")));
        }
    }

    @Test
    public void testAllLabelsAndExpressionsAreConjunctive() {
        JsonObject selector = selector("{\"matchLabels\":{\"stage\":\"production\"},\"matchExpressions\":["
                + "{\"key\":\"team\",\"operator\":\"In\",\"values\":[\"blue\",\"green\"]},"
                + "{\"key\":\"team\",\"operator\":\"NotIn\",\"values\":[\"green\"]},"
                + "{\"key\":\"enabled\",\"operator\":\"Exists\"},"
                + "{\"key\":\"blocked\",\"operator\":\"DoesNotExist\"}]}");
        Assertions.assertTrue(NamespaceSelector.matches(selector, Map.of("stage", "production", "team", "blue", "enabled", "")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("stage", "development", "team", "blue", "enabled", "")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("stage", "production", "team", "green", "enabled", "")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("stage", "production", "team", "blue")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("stage", "production", "team", "blue", "enabled", "", "blocked", "")));
    }

    @Test
    public void testMatchingDoesNotChangeSelectorOrLabels() {
        JsonObject selector = expression("team", "In", "blue", "green");
        JsonObject original = selector.deepCopy();
        Map<String, String> labels = Map.of("team", "blue");
        Assertions.assertTrue(NamespaceSelector.matches(selector, labels));
        Assertions.assertEquals(original, selector);
        Assertions.assertEquals(Map.of("team", "blue"), labels);
        Assertions.assertTrue(NamespaceSelector.matches(selector, labels));
    }

    @Test
    public void testContradictoryRequirementsDoNotMatch() {
        JsonObject selector = expression("team", "DoesNotExist");
        selector.add("matchLabels", matchLabels("team", "blue").get("matchLabels"));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of("team", "blue")));
        Assertions.assertFalse(NamespaceSelector.matches(selector, Map.of()));
    }

    @Test
    public void testInAndNotInRequireNonemptyValues() {
        for (String operator : new String[]{"In", "NotIn"}) {
            Assertions.assertFalse(NamespaceSelector.matches(expression("team", operator), Map.of()));
            Assertions.assertFalse(NamespaceSelector.matches(expression("team", operator), Map.of("team", "blue")));
            JsonObject omitted = expression("team", operator, "blue");
            omitted.getAsJsonArray("matchExpressions").get(0).getAsJsonObject().remove("values");
            Assertions.assertFalse(NamespaceSelector.matches(omitted, Map.of()));
            Assertions.assertFalse(NamespaceSelector.matches(omitted, Map.of("team", "blue")));
        }
    }

    @Test
    public void testExistenceOperatorsRejectNonemptyValues() {
        for (String operator : new String[]{"Exists", "DoesNotExist"}) {
            for (String value : new String[]{"", "blue"}) {
                Assertions.assertFalse(NamespaceSelector.matches(expression("team", operator, value), Map.of()));
                Assertions.assertFalse(NamespaceSelector.matches(expression("team", operator, value), Map.of("team", "blue")));
            }
        }
    }

    @Test
    public void testUnknownOperatorsFailClosed() {
        for (String operator : new String[]{"", "in", "NOTIN", "Equals", "Gt", "Lt", "Exists "}) {
            Assertions.assertFalse(NamespaceSelector.matches(expression("team", operator, "blue"), Map.of("team", "blue")), operator);
            Assertions.assertFalse(NamespaceSelector.matches(expression("team", operator, "blue"), Map.of()), operator);
        }
    }

    @Test
    public void testMalformedOptionalCollectionsFailClosedWithoutThrowing() {
        for (String value : new String[]{"null", "true", "42", "\"blue\"", "[]"}) {
            assertRejected("{\"matchLabels\":" + value + "}");
        }
        for (String value : new String[]{"null", "false", "42", "\"blue\"", "{}"}) {
            assertRejected("{\"matchExpressions\":" + value + "}");
        }
        for (String value : new String[]{"null", "true", "42", "\"blue\"", "[]"}) {
            assertRejected("{\"matchExpressions\":[" + value + "]}");
        }
    }

    @Test
    public void testMissingOrMistypedExpressionFieldsFailClosedWithoutThrowing() {
        for (String json : new String[]{"{}", "{\"key\":\"team\"}", "{\"operator\":\"Exists\"}"}) {
            assertRejected("{\"matchExpressions\":[" + json + "]}");
        }
        for (String value : new String[]{"null", "true", "42", "{}", "[]"}) {
            assertRejected("{\"matchExpressions\":[{\"key\":" + value + ",\"operator\":\"Exists\"}]}");
            assertRejected("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":" + value + "}]}");
        }
        for (String value : new String[]{"42", "true"}) {
            JsonObject mistyped = selector("{\"matchExpressions\":[{\"key\":" + value + ",\"operator\":\"Exists\"}]}");
            Assertions.assertFalse(NamespaceSelector.matches(mistyped, Map.of(value, "blue")));
            Assertions.assertTrue(NamespaceSelector.matches(expression(value, "Exists"), Map.of(value, "blue")));
        }
    }

    @Test
    public void testMalformedExpressionValuesFailClosedWithoutThrowing() {
        for (String operator : new String[]{"In", "NotIn", "Exists", "DoesNotExist"}) {
            for (String value : new String[]{"null", "true", "42", "\"blue\"", "{}"}) {
                assertRejected("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"" + operator + "\",\"values\":" + value + "}]}");
            }
            for (String value : new String[]{"null", "true", "42", "{}", "[]"}) {
                assertRejected("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"" + operator + "\",\"values\":[\"blue\"," + value + "]}]}");
            }
        }
    }

    @Test
    public void testMatchLabelValuesAreStringsWithoutGsonCoercion() {
        for (String value : new String[]{"null", "true", "42", "{}", "[]"}) {
            assertRejected("{\"matchLabels\":{\"team\":" + value + "}}");
        }
        for (String value : new String[]{"42", "true"}) {
            Assertions.assertFalse(NamespaceSelector.matches(selector("{\"matchLabels\":{\"team\":" + value + "}}"), Map.of("team", value)));
            JsonObject numericOrBoolean = selector("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"In\",\"values\":[" + value + "]}]}");
            Assertions.assertFalse(NamespaceSelector.matches(numericOrBoolean, Map.of("team", value)));
            Assertions.assertTrue(NamespaceSelector.matches(matchLabels("team", value), Map.of("team", value)));
            Assertions.assertTrue(NamespaceSelector.matches(expression("team", "In", value), Map.of("team", value)));
        }
    }

    @Test
    public void testUnknownFieldsCannotBecomeMatchAllSelectors() {
        assertRejected("{\"matchLabel\":{\"team\":\"blue\"}}");
        assertRejected("{\"matchLabels\":{},\"unknown\":null}");
        assertRejected("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"Exists\",\"value\":\"blue\"}]}");
    }

    @Test
    public void testMalformedRequirementCannotBeHiddenByMatchingRequirements() {
        assertRejected("{\"matchLabels\":{\"team\":\"blue\"},\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"Exists\"},{}]}");
        assertRejected("{\"matchLabels\":{\"team\":\"blue\",\"invalid/key/again\":\"blue\"}}");
        assertRejected("{\"matchExpressions\":[{\"key\":\"team\",\"operator\":\"In\",\"values\":[\"blue\",42]}]}");
        assertRejected("{\"matchExpressions\":[{\"key\":\"absent\",\"operator\":\"NotIn\",\"values\":[\"blue\",null]}]}");
    }

    @Test
    public void testQualifiedKeyAndNameLengthBoundaries() {
        String prefix = "a".repeat(63) + "." + "b".repeat(63) + "." + "c".repeat(63) + "." + "d".repeat(61);
        for (String key : new String[]{"a", "A", "team_name", "team.name", "team-name", "example.com/Team_1", "a".repeat(63), prefix + "/" + "N".repeat(63)}) {
            Assertions.assertTrue(NamespaceSelector.matches(matchLabels(key, "blue"), Map.of(key, "blue")), key);
            Assertions.assertTrue(NamespaceSelector.matches(expression(key, "In", "blue"), Map.of(key, "blue")), key);
        }
        Assertions.assertFalse(NamespaceSelector.matches(matchLabels("a".repeat(64), "blue"), Map.of("a".repeat(64), "blue")));
        Assertions.assertFalse(NamespaceSelector.matches(expression("a".repeat(64), "Exists"), Map.of("a".repeat(64), "blue")));
        String tooLongPrefix = prefix + "d/team";
        Assertions.assertFalse(NamespaceSelector.matches(matchLabels(tooLongPrefix, "blue"), Map.of(tooLongPrefix, "blue")));
        Assertions.assertFalse(NamespaceSelector.matches(expression(tooLongPrefix, "NotIn", "red"), Map.of()));
    }

    @Test
    public void testInvalidKeysFailClosedInLabelsAndExpressions() {
        for (String key : new String[]{"", "/team", "example.com/", "example.com/team/other", "Example.com/team", "example_com/team", "-example.com/team",
                "example-.com/team", "example..com/team", "example.com./team", "-team", "team-", "_team", "team_", ".team", "team.", "team name", "téam"}) {
            Assertions.assertFalse(NamespaceSelector.matches(matchLabels(key, "blue"), Map.of(key, "blue")), key);
            Assertions.assertFalse(NamespaceSelector.matches(expression(key, "In", "blue"), Map.of(key, "blue")), key);
            Assertions.assertFalse(NamespaceSelector.matches(expression(key, "NotIn", "red"), Map.of()), key);
        }
    }

    @Test
    public void testValidLabelValuesIncludeEmptyAndLengthBoundary() {
        for (String value : new String[]{"", "a", "A_1.example-test", "a".repeat(63)}) {
            Assertions.assertTrue(NamespaceSelector.matches(matchLabels("team", value), Map.of("team", value)), value);
            Assertions.assertTrue(NamespaceSelector.matches(expression("team", "In", value), Map.of("team", value)), value);
            Assertions.assertFalse(NamespaceSelector.matches(expression("team", "NotIn", value), Map.of("team", value)), value);
            Assertions.assertTrue(NamespaceSelector.matches(expression("team", "NotIn", value), Map.of()), value);
        }
        Assertions.assertFalse(NamespaceSelector.matches(matchLabels("team", ""), Map.of()));
        Assertions.assertFalse(NamespaceSelector.matches(expression("team", "In", ""), Map.of()));
        Assertions.assertTrue(NamespaceSelector.matches(expression("team", "In", "blue", "blue"), Map.of("team", "blue")));
    }

    @Test
    public void testInvalidLabelValuesFailClosed() {
        for (String value : new String[]{"a".repeat(64), "-blue", "blue-", "_blue", "blue_", ".blue", "blue.", "blue team", "blue/team", "blüe"}) {
            Assertions.assertFalse(NamespaceSelector.matches(matchLabels("team", value), Map.of("team", value)), value);
            Assertions.assertFalse(NamespaceSelector.matches(expression("team", "In", value), Map.of("team", value)), value);
            Assertions.assertFalse(NamespaceSelector.matches(expression("team", "NotIn", value), Map.of()), value);
        }
    }

    private static JsonObject selector(final String json) {
        return JsonParser.parseString(json).getAsJsonObject();
    }

    private static JsonObject matchLabels(final String key, final String value) {
        JsonObject labels = new JsonObject();
        labels.addProperty(key, value);
        JsonObject selector = new JsonObject();
        selector.add("matchLabels", labels);
        return selector;
    }

    private static JsonObject expression(final String key, final String operator, final String... values) {
        JsonObject requirement = new JsonObject();
        requirement.addProperty("key", key);
        requirement.addProperty("operator", operator);
        JsonArray array = new JsonArray();
        for (String value : values) {
            array.add(value);
        }
        requirement.add("values", array);
        JsonArray expressions = new JsonArray();
        expressions.add(requirement);
        JsonObject selector = new JsonObject();
        selector.add("matchExpressions", expressions);
        return selector;
    }

    private static void assertRejected(final String json) {
        JsonElement parsed = JsonParser.parseString(json);
        JsonObject selector = parsed.getAsJsonObject();
        Assertions.assertFalse(Assertions.assertDoesNotThrow(() -> NamespaceSelector.matches(selector, Map.of("team", "blue"))), json);
        Assertions.assertFalse(Assertions.assertDoesNotThrow(() -> NamespaceSelector.matches(selector, Map.of())), json);
    }
}
