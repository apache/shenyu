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

package org.apache.shenyu.admin.service.openapi.impl;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.admin.mapper.AgentGatewayImportMapper;
import org.apache.shenyu.admin.mapper.RuleConditionMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Apply;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Mapping;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.AgentGatewayImportDO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.query.RuleConditionQuery;
import org.apache.shenyu.admin.model.query.SelectorConditionQuery;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Item;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.openapi.AgentGatewayOpenApiDiff;
import org.apache.shenyu.admin.service.openapi.AgentGatewayOpenApiGenerator;
import org.apache.shenyu.admin.service.openapi.AgentGatewayOpenApiService;
import org.apache.shenyu.admin.validation.validator.AgentGatewayRuleHandleValidator;
import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

/** Preview and explicit import through the existing rule publication flow. */
@Service
public class AgentGatewayOpenApiServiceImpl implements AgentGatewayOpenApiService {

    private final RuleMapper rules;

    private final SelectorMapper selectors;

    private final RuleConditionMapper ruleConditions;

    private final SelectorConditionMapper selectorConditions;

    private final AgentGatewayImportMapper imports;

    private final RuleService ruleService;

    private final Set<String> reservedNames;

    public AgentGatewayOpenApiServiceImpl(final RuleMapper rules, final SelectorMapper selectors, final RuleConditionMapper ruleConditions,
                                          final SelectorConditionMapper selectorConditions, final AgentGatewayImportMapper imports, final RuleService ruleService,
                                          @Value("${shenyu.agent-gateway.openapi.reserved-tool-names:}") final String reservedNames) {
        this.rules = rules;
        this.selectors = selectors;
        this.ruleConditions = ruleConditions;
        this.selectorConditions = selectorConditions;
        this.imports = imports;
        this.ruleService = ruleService;
        this.reservedNames = Arrays.stream(reservedNames.split(",")).map(String::trim).filter(value -> !value.isEmpty()).collect(Collectors.toUnmodifiableSet());
    }

    @Override
    public AgentGatewayOpenApiPreview preview(final AgentGatewayOpenApiRequest request) {
        return prepare(request).preview();
    }

    /**
     * Recheck the complete preview after locking the target rule, then publish selected changes.
     * @param apply exact preview request, revision and selected source identities
     * @return new preview after successful publication
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public AgentGatewayOpenApiPreview apply(final Apply apply) {
        require(Objects.nonNull(apply.selected()) && !apply.selected().isEmpty(), "Select at least one preview item");
        require(Objects.nonNull(rules.selectByIdForUpdate(apply.request().ruleId())), "Target rule no longer exists");
        Prepared prepared = prepare(apply.request());
        require(prepared.preview().revision().equals(apply.revision()), "Preview is stale; preview again before publishing");
        require(apply.acknowledgeCatalogLimit(), "Acknowledge the preview's external provider catalog limitation before publishing");
        List<Item> selected = prepared.preview().items().stream().filter(item -> apply.selected().contains(item.identity())).toList();
        require(selected.size() == apply.selected().size(), "Selection contains an unknown preview identity");
        require(selected.stream().allMatch(item -> Set.of("ADD", "UPDATE", "REMOVE", "UNCHANGED").contains(item.action())), "Selection contains unsupported or conflicting operations");
        require(apply.confirmRemovals() || selected.stream().noneMatch(item -> "REMOVE".equals(item.action())), "Source removals require explicit confirmation");
        JsonObject handle = prepared.handle().deepCopy();
        JsonObject mcp = handle.getAsJsonObject("mcp");
        JsonObject tools = mcp.has("httpTools") ? mcp.getAsJsonObject("httpTools") : new JsonObject();
        Set<String> allowed = new LinkedHashSet<>();
        if (mcp.has("allowedTools")) {
            mcp.getAsJsonArray("allowedTools").forEach(value -> allowed.add(value.getAsString()));
        }
        Set<String> finalTools = new LinkedHashSet<>(tools.keySet());
        selected.forEach(item -> {
            if ("REMOVE".equals(item.action())) {
                finalTools.remove(item.toolName());
            } else if (!"UNCHANGED".equals(item.action())) {
                finalTools.add(item.toolName());
            }
        });
        require(finalTools.size() <= 16, "Tool budget exceeded: the target rule has " + tools.size()
                + " httpTools and the selection would produce " + finalTools.size()
                + "; the runtime maximum is 16 tools per rule");
        JsonObject manifest = prepared.manifest().deepCopy();
        JsonObject owned = manifest.has(apply.request().sourceId()) ? manifest.getAsJsonObject(apply.request().sourceId()) : new JsonObject();
        for (Item item : selected) {
            change(item, tools, allowed, owned, mcp, apply.request());
        }
        manifest.add(apply.request().sourceId(), owned);
        mcp.add("httpTools", tools);
        JsonArray permissions = new JsonArray();
        allowed.forEach(permissions::add);
        mcp.add("allowedTools", permissions);
        AgentGatewayRuleHandleValidator.validate(handle.toString());
        require(manifest.toString().getBytes(StandardCharsets.UTF_8).length <= 1048576, "Import provenance exceeds the 1 MiB bound");
        boolean handleChanged = !handle.equals(prepared.handle());
        // Persist provenance first: a publication failure rolls it back with the rule update.
        Timestamp now = new Timestamp(System.currentTimeMillis());
        if (Objects.nonNull(prepared.provenance())) {
            require(imports.updateManifestById(prepared.provenance().getId(), manifest.toString(), now) == 1, "Cannot save import provenance");
        } else {
            AgentGatewayImportDO record = new AgentGatewayImportDO();
            record.setId(UUIDUtils.getInstance().generateShortUuid());
            record.setRuleId(apply.request().ruleId());
            record.setNamespaceId(apply.request().namespaceId());
            record.setManifest(manifest.toString());
            record.setDateCreated(now);
            record.setDateUpdated(now);
            require(imports.insertSelective(record) == 1, "Cannot save import provenance");
        }
        if (handleChanged) {
            RuleDTO update = update(prepared.rule(), handle.toString());
            require(ruleService.createOrUpdate(update) == 1, "Rule publication failed");
        }
        return prepare(apply.request()).preview();
    }

    private Prepared prepare(final AgentGatewayOpenApiRequest request) {
        require(Objects.nonNull(request) && Objects.nonNull(request.sourceId()) && !request.sourceId().isBlank() && request.sourceId().length() <= 128, "Explicit sourceId is required");
        RuleDO rule = rules.selectById(request.ruleId());
        require(Objects.nonNull(rule) && Objects.equals(request.namespaceId(), rule.getNamespaceId()), "Target rule must exist in the exact namespace");
        SelectorDO selector = selectors.selectById(rule.getSelectorId());
        require(Objects.nonNull(selector) && Objects.equals(request.namespaceId(), selector.getNamespaceId())
                && "agentGateway".equals(rules.getPluginNameBySelectorId(rule.getSelectorId())), "Target must be an Agent Gateway rule in the exact namespace");
        AgentGatewayRuleHandleValidator.validate(rule.getHandle());
        JsonObject handle = JsonParser.parseString(rule.getHandle()).getAsJsonObject();
        require("mcp".equals(handle.get("trafficType").getAsString()), "Target rule must already use trafficType mcp");
        JsonObject mcp = handle.getAsJsonObject("mcp");
        Set<String> allowed = AgentGatewayMcpConfig.parse(mcp, true).getAllowedTools();
        JsonObject tools = mcp.has("httpTools") ? mcp.getAsJsonObject("httpTools") : new JsonObject();
        AgentGatewayImportDO provenance = imports.selectByRuleId(rule.getId(), request.namespaceId());
        JsonObject manifest = Objects.isNull(provenance) ? new JsonObject() : JsonParser.parseString(provenance.getManifest()).getAsJsonObject();
        List<Item> items = new ArrayList<>(new AgentGatewayOpenApiDiff().preview(new AgentGatewayOpenApiGenerator().generate(request), tools, manifest, request.sourceId()));
        JsonArray routing = new JsonArray();
        for (int index = 0; index < items.size(); index++) {
            Item item = items.get(index);
            if (Objects.isNull(item.after()) || Set.of("CONFLICT", "UNSUPPORTED").contains(item.action())) {
                continue;
            }
            Mapping mapping = request.mappings().stream().filter(value -> value.identity().equals(item.identity())).findFirst().orElseThrow();
            try {
                if (reservedNames.contains(item.toolName()) || "ADD".equals(item.action()) && allowed.contains(item.toolName())) {
                    throw new IllegalArgumentException("Tool name is reserved by an existing provider/allowedTools binding; choose a different toolName");
                }
                routing.add(route(request.namespaceId(), mapping));
            } catch (IllegalArgumentException error) {
                items.set(index, new Item(item.identity(), item.toolName(), "CONFLICT", item.before(), item.after(), List.of(error.getMessage())));
            }
        }
        JsonObject snapshot = new JsonObject();
        snapshot.add("request", json(request));
        snapshot.add("rule", json(rule));
        snapshot.add("selector", json(selector));
        snapshot.add("manifest", manifest);
        snapshot.add("routing", routing);
        snapshot.add("items", json(items));
        snapshot.add("reservedNames", json(reservedNames.stream().sorted().toList()));
        List<String> warnings = List.of("EXTERNAL_CATALOG_UNVERIFIED: Admin checks configured HTTP tools, existing allowedTools bindings and operator-configured reserved names. "
                + "Startup Java providers and live remote MCP catalogs are not available here; verify their names before applying. Runtime collision guards remain authoritative.",
                "Selected additions add allowedTools bindings. Selected removals remove only import-created grants and the named result filter; all other grants are preserved.");
        return new Prepared(rule, handle, manifest, provenance, new AgentGatewayOpenApiPreview(digest(canonical(snapshot).toString()), List.copyOf(items), warnings));
    }

    private JsonObject route(final String namespace, final Mapping mapping) {
        RuleDO rule = rules.selectById(mapping.gatewayRuleId());
        require(Objects.nonNull(rule) && Objects.equals(namespace, rule.getNamespaceId()) && Boolean.TRUE.equals(rule.getEnabled()), "Mapped gateway route must be enabled in the exact namespace");
        require(Set.of("divide", "springCloud").contains(rules.getPluginNameBySelectorId(rule.getSelectorId())), "Mapped rule must belong to the divide or springCloud HTTP routing plugin");
        SelectorDO selector = selectors.selectById(rule.getSelectorId());
        require(Objects.nonNull(selector) && Objects.equals(namespace, selector.getNamespaceId()) && Boolean.TRUE.equals(selector.getEnabled()),
                "Mapped routing selector must be enabled in the exact namespace");
        List<RuleConditionDO> conditions = ruleConditions.selectByQuery(new RuleConditionQuery(rule.getId()));
        require(Objects.equals(rule.getMatchMode(), 0) && conditions.stream().anyMatch(value -> "uri".equals(value.getParamType())
                && "=".equals(value.getOperator()) && mapping.gatewayPath().equals(value.getParamValue())), "Mapped routing rule requires an AND condition with the exact gateway URI");
        require(conditions.stream().allMatch(value -> matches(value.getParamType(), value.getOperator(), value.getParamValue(), mapping)),
                "Mapped routing rule has unsupported or mismatched conditions");
        List<SelectorConditionDO> selectorFilters = selectorConditions.selectByQuery(new SelectorConditionQuery(selector.getId()));
        boolean selectorMatches = Objects.equals(selector.getSelectorType(), 0) || !selectorFilters.isEmpty()
                && (Objects.equals(selector.getMatchMode(), 0)
                ? selectorFilters.stream().allMatch(value -> matches(value.getParamType(), value.getOperator(), value.getParamValue(), mapping))
                : selectorFilters.stream().allMatch(value -> supported(value.getParamType(), value.getOperator()))
                && selectorFilters.stream().anyMatch(value -> matches(value.getParamType(), value.getOperator(), value.getParamValue(), mapping)));
        require(selectorMatches, "Mapped selector must be full-flow or match supported URI/method conditions");
        JsonObject snapshot = new JsonObject();
        snapshot.add("rule", json(rule));
        snapshot.add("selector", json(selector));
        snapshot.add("ruleConditions", json(conditions));
        snapshot.add("selectorConditions", json(selectorFilters));
        return snapshot;
    }

    private boolean supported(final String type, final String operator) {
        return "uri".equals(type) && Set.of("=", "startsWith").contains(operator) || "req_method".equals(type) && "=".equals(operator);
    }

    private boolean matches(final String type, final String operator, final String value, final Mapping mapping) {
        if (!supported(type, operator) || Objects.isNull(value)) {
            return false;
        }
        String actual = "uri".equals(type) ? mapping.gatewayPath() : mapping.method();
        return "=".equals(operator) ? actual.equals(value) : actual.startsWith(value);
    }

    private void change(final Item item, final JsonObject tools, final Set<String> allowed, final JsonObject owned, final JsonObject mcp, final AgentGatewayOpenApiRequest request) {
        JsonObject previous = owned.has(item.identity()) ? owned.getAsJsonObject(item.identity()) : null;
        if ("REMOVE".equals(item.action())) {
            tools.remove(item.toolName());
            if (granted(previous)) {
                allowed.remove(item.toolName());
            }
            if (mcp.has("structuredContentRemovals")) {
                mcp.getAsJsonObject("structuredContentRemovals").remove(item.toolName());
            }
            owned.remove(item.identity());
            return;
        }
        JsonObject plan = JsonParser.parseString(item.after()).getAsJsonObject();
        tools.add(item.toolName(), plan);
        JsonObject entry = new JsonObject();
        entry.addProperty("name", item.toolName());
        entry.add("plan", plan.deepCopy());
        entry.addProperty("grantAdded", Objects.nonNull(previous) ? granted(previous) : !allowed.contains(item.toolName()));
        entry.addProperty("gatewayRuleId", request.mappings().stream().filter(value -> value.identity().equals(item.identity())).findFirst().orElseThrow().gatewayRuleId());
        owned.add(item.identity(), entry);
        allowed.add(item.toolName());
    }

    private boolean granted(final JsonObject entry) {
        if (Objects.isNull(entry) || !entry.has("grantAdded")) {
            return false;
        }
        JsonElement value = entry.get("grantAdded");
        return value.isJsonPrimitive() && value.getAsJsonPrimitive().isBoolean() && value.getAsBoolean();
    }

    private RuleDTO update(final RuleDO rule, final String handle) {
        List<RuleConditionDTO> conditions = ruleConditions.selectByQuery(new RuleConditionQuery(rule.getId())).stream().map(value -> RuleConditionDTO.builder()
                .ruleId(rule.getId()).paramType(value.getParamType()).paramName(value.getParamName()).paramValue(value.getParamValue()).operator(value.getOperator()).build()).toList();
        return new RuleDTO(rule.getId(), rule.getSelectorId(), rule.getMatchMode(), rule.getRuleName(), rule.getEnabled(), rule.getLogged(), rule.getSortCode(), handle,
                conditions, rule.getMatchRestful(), rule.getNamespaceId());
    }

    private JsonElement json(final Object value) {
        return JsonParser.parseString(GsonUtils.getInstance().toJson(value));
    }

    private JsonElement canonical(final JsonElement value) {
        if (value.isJsonObject()) {
            Map<String, JsonElement> sorted = new TreeMap<>(value.getAsJsonObject().asMap());
            JsonObject result = new JsonObject();
            sorted.forEach((key, entry) -> result.add(key, canonical(entry)));
            return result;
        }
        if (value.isJsonArray()) {
            JsonArray result = new JsonArray();
            value.getAsJsonArray().forEach(entry -> result.add(canonical(entry)));
            return result;
        }
        return value;
    }

    private String digest(final String value) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException error) {
            throw new IllegalStateException("SHA-256 unavailable", error);
        }
    }

    private static void require(final boolean valid, final String message) {
        if (!valid) {
            throw new IllegalArgumentException(message);
        }
    }

    private record Prepared(RuleDO rule, JsonObject handle, JsonObject manifest, AgentGatewayImportDO provenance,
                            AgentGatewayOpenApiPreview preview) {
    }
}
