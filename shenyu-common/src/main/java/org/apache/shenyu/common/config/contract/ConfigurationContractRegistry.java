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

package org.apache.shenyu.common.config.contract;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import org.apache.shenyu.common.utils.GsonUtils;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.math.BigDecimal;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.ServiceLoader;
import java.util.concurrent.ConcurrentHashMap;

import static org.apache.shenyu.common.config.contract.ConfigurationValidation.Status.INVALID;
import static org.apache.shenyu.common.config.contract.ConfigurationValidation.Status.UNREGISTERED;
import static org.apache.shenyu.common.config.contract.ConfigurationValidation.Status.UNSUPPORTED_VERSION;
import static org.apache.shenyu.common.config.contract.ConfigurationValidation.Status.VALID;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.INVALID_ENUM;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.INVALID_JSON;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.INVALID_TYPE;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.NULL_NOT_ALLOWED;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.OUT_OF_RANGE;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.PROVIDER_FAILURE;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.REQUIRED;
import static org.apache.shenyu.common.config.contract.ConfigurationViolation.Code.UNKNOWN_FIELD;

/**
 * Optional shared configuration registry. No request-path parsing or cache-update policy is imposed.
 */
public final class ConfigurationContractRegistry {

    private static final ObjectMapper STRICT_JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private final Map<Key, Registration> providers = new ConcurrentHashMap<>();

    /**
     * Shared providers discovered from lightweight contract artifacts only.
     * @return shared registry
     */
    public static ConfigurationContractRegistry shared() {
        return Shared.INSTANCE;
    }

    /**
     * Register one immutable descriptor and its pure semantic validator at startup.
     * @param provider trusted lightweight provider
     */
    public synchronized void register(final ConfigurationContractProvider provider) {
        ConfigurationContract contract = provider.contract();
        Key key = new Key(contract.plugin(), contract.scope(), contract.version());
        if (providers.size() >= 1024 || Objects.nonNull(providers.putIfAbsent(key, new Registration(contract, provider)))) {
            throw new IllegalArgumentException("Duplicate configuration contract or registry limit exceeded");
        }
    }

    /**
     * Inspect installed metadata without invoking semantic validators or external providers.
     * @return immutable deterministic catalog
     */
    public List<ConfigurationContract> catalog() {
        return providers.values().stream().map(Registration::contract)
                .sorted(Comparator.comparing(ConfigurationContract::plugin).thenComparing(value -> value.scope().name())
                        .thenComparing(ConfigurationContract::version)).toList();
    }

    /**
     * Validate against an explicit contract. Unregistered legacy input is left untouched.
     * @param plugin trusted stable plugin identity
     * @param scope stored configuration scope
     * @param version explicit schema version, independent of configuration revision
     * @param source original JSON, never rewritten or default-filled
     * @return bounded validation facts
     */
    public ConfigurationValidation validate(final String plugin, final ConfigurationScope scope, final String version, final String source) {
        Registration registration = providers.get(new Key(plugin, scope, version));
        if (Objects.isNull(registration)) {
            boolean registeredScope = providers.keySet().stream().anyMatch(key -> key.plugin().equals(plugin) && key.scope() == scope);
            return new ConfigurationValidation(plugin, scope, version, registeredScope ? UNSUPPORTED_VERSION : UNREGISTERED, List.of());
        }
        List<ConfigurationViolation> errors = new ArrayList<>();
        try {
            ConfigurationContract contract = registration.contract();
            if (contract.structuralValidation()) {
                validateStructure(source, contract, errors);
            }
            if (errors.isEmpty()) {
                errors.addAll(registration.provider().validateSemantic(source).stream().limit(16).toList());
            }
        } catch (RuntimeException error) {
            // Do not attach parser/provider exceptions: messages and causes may contain secrets.
            errors = List.of(new ConfigurationViolation("$", PROVIDER_FAILURE));
        }
        return new ConfigurationValidation(plugin, scope, version, errors.isEmpty() ? VALID : INVALID, errors);
    }

    private void validateStructure(final String source, final ConfigurationContract contract, final List<ConfigurationViolation> errors) {
        final JsonElement root;
        try {
            root = GsonUtils.getInstance().fromJson(STRICT_JSON.readTree(source).toString(), JsonElement.class);
        } catch (IOException | JsonParseException | IllegalStateException | IllegalArgumentException error) {
            errors.add(new ConfigurationViolation("$", INVALID_JSON));
            return;
        }
        if (Objects.isNull(root) || !root.isJsonObject()) {
            errors.add(new ConfigurationViolation("$", INVALID_TYPE));
            return;
        }
        JsonObject object = root.getAsJsonObject();
        for (Map.Entry<String, ConfigurationContract.Field> entry : contract.fields().entrySet()) {
            if (errors.size() >= 16) {
                return;
            }
            validateField(entry.getKey(), entry.getValue(), lookup(object, entry.getKey()), errors);
        }
        if (contract.unknownFields() == ConfigurationContract.UnknownFields.REJECT && object.keySet().stream()
                .anyMatch(name -> contract.fields().keySet().stream().noneMatch(path -> path.equals(name) || path.startsWith(name + ".")))) {
            // Unknown names are untrusted, and may themselves be credential values.
            errors.add(new ConfigurationViolation("$", UNKNOWN_FIELD));
        }
    }

    private JsonElement lookup(final JsonObject root, final String path) {
        JsonElement value = root;
        for (String segment : path.split("\\.")) {
            if (Objects.isNull(value) || !value.isJsonObject()) {
                return null;
            }
            value = value.getAsJsonObject().get(segment);
        }
        return value;
    }

    private void validateField(final String path, final ConfigurationContract.Field field, final JsonElement value,
                               final List<ConfigurationViolation> errors) {
        if (Objects.isNull(value)) {
            if (field.required()) {
                errors.add(new ConfigurationViolation(path, REQUIRED));
            }
            return;
        }
        if (value.isJsonNull()) {
            if (!field.nullable()) {
                errors.add(new ConfigurationViolation(path, NULL_NOT_ALLOWED));
            }
            return;
        }
        if (!matchesType(field.type(), value)) {
            errors.add(new ConfigurationViolation(path, INVALID_TYPE));
            return;
        }
        if (value.isJsonPrimitive() && value.getAsJsonPrimitive().isNumber()) {
            BigDecimal number = value.getAsBigDecimal();
            if (Objects.nonNull(field.minimum()) && number.compareTo(BigDecimal.valueOf(field.minimum())) < 0
                    || Objects.nonNull(field.maximum()) && number.compareTo(BigDecimal.valueOf(field.maximum())) > 0) {
                errors.add(new ConfigurationViolation(path, OUT_OF_RANGE));
            }
        }
        if (!field.values().isEmpty() && (!value.isJsonPrimitive() || !value.getAsJsonPrimitive().isString()
                || !field.values().contains(value.getAsString()))) {
            errors.add(new ConfigurationViolation(path, INVALID_ENUM));
        }
    }

    private boolean matchesType(final ConfigurationContract.Type type, final JsonElement value) {
        return switch (type) {
            case STRING -> value.isJsonPrimitive() && value.getAsJsonPrimitive().isString();
            case BOOLEAN -> value.isJsonPrimitive() && value.getAsJsonPrimitive().isBoolean();
            case OBJECT -> value.isJsonObject();
            case ARRAY -> value.isJsonArray();
            case NUMBER -> value.isJsonPrimitive() && value.getAsJsonPrimitive().isNumber();
            case INTEGER -> value.isJsonPrimitive() && value.getAsJsonPrimitive().isNumber()
                    && value.getAsBigDecimal().stripTrailingZeros().scale() <= 0;
        };
    }

    private record Key(String plugin, ConfigurationScope scope, String version) {
    }

    private record Registration(ConfigurationContract contract, ConfigurationContractProvider provider) {
    }

    private static final class Shared {

        private static final ConfigurationContractRegistry INSTANCE = load();

        private static ConfigurationContractRegistry load() {
            ConfigurationContractRegistry registry = new ConfigurationContractRegistry();
            ServiceLoader.load(ConfigurationContractProvider.class, ConfigurationContractProvider.class.getClassLoader()).forEach(registry::register);
            return registry;
        }
    }
}
