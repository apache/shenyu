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

package org.apache.shenyu.admin.service.openapi;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Generated;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview.Item;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/** Ownership-aware structural diff, independent of persistence and publication. */
public final class AgentGatewayOpenApiDiff {

    /**
     * Diff selected operations and true source deletions, preserving deselected operations.
     * @param generated selected plans and complete source identities
     * @param current current runtime tool map
     * @param manifest all source ownership records for this target rule
     * @param sourceId current import source
     * @return selectable changes or visible conflicts
     */
    public List<Item> preview(final Generated generated, final JsonObject current, final JsonObject manifest, final String sourceId) {
        JsonObject owned = manifest.has(sourceId) ? manifest.getAsJsonObject(sourceId) : new JsonObject();
        List<Item> result = new ArrayList<>();
        generated.items().forEach((identity, item) -> {
            if (Objects.isNull(item.after())) {
                result.add(item);
                return;
            }
            JsonObject previous = owned.has(identity) ? owned.getAsJsonObject(identity) : null;
            if (Objects.nonNull(previous)) {
                String oldName = previous.get("name").getAsString();
                if (!oldName.equals(item.toolName())) {
                    result.add(conflict(item, current.get(oldName), "Changing an imported tool name requires explicit removal and a new import identity/source"));
                } else if (!Objects.equals(current.get(oldName), previous.get("plan"))) {
                    result.add(conflict(item, current.get(oldName), "Previously imported tool was manually edited or removed; restore it or manage it manually"));
                } else {
                    String action = current.get(oldName).equals(JsonParser.parseString(item.after())) ? "UNCHANGED" : "UPDATE";
                    result.add(new Item(identity, oldName, action, current.get(oldName).toString(), item.after(), item.diagnostics()));
                }
            } else if (current.has(item.toolName()) || claimed(manifest, item.toolName())) {
                result.add(conflict(item, current.get(item.toolName()), "Name belongs to a manual tool or another import; choose a different toolName"));
            } else {
                result.add(new Item(identity, item.toolName(), "ADD", null, item.after(), item.diagnostics()));
            }
        });
        owned.entrySet().forEach(entry -> {
            if (!generated.sourceIdentities().contains(entry.getKey())) {
                JsonObject previous = entry.getValue().getAsJsonObject();
                String name = previous.get("name").getAsString();
                Item removal = new Item(entry.getKey(), name, "REMOVE", previous.get("plan").toString(), null, List.of("Operation no longer exists in the supplied source document"));
                result.add(Objects.equals(current.get(name), previous.get("plan")) ? removal : conflict(removal, current.get(name), "Manually edited or removed tool; automatic removal is blocked"));
            }
        });
        return List.copyOf(result);
    }

    private boolean claimed(final JsonObject manifest, final String name) {
        return manifest.entrySet().stream().anyMatch(source -> source.getValue().getAsJsonObject().entrySet().stream()
                .anyMatch(entry -> name.equals(entry.getValue().getAsJsonObject().get("name").getAsString())));
    }

    private Item conflict(final Item item, final JsonElement before, final String message) {
        return new Item(item.identity(), item.toolName(), "CONFLICT", Objects.isNull(before) ? null : before.toString(), item.after(), List.of(message));
    }
}
