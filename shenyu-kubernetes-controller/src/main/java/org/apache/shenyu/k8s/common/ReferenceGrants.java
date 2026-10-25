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
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.commons.collections4.CollectionUtils;

import java.util.List;
import java.util.Objects;

/**
 * ReferenceGrant (GEP-709) matching for cross-namespace references: a grant lives in
 * the namespace of the referenced resource and allows an HTTPRoute from one namespace
 * to reference a resource of a given group/kind (optionally restricted by name).
 */
public final class ReferenceGrants {

    private ReferenceGrants() {
    }

    /**
     * Whether a grant in grantNamespace (where referenced resources' grants live) allows the reference; to.name restricts to one resource.
     *
     * @param grantLister lister for ReferenceGrant resources
     * @param grantNamespace namespace of the referenced resource, where grants live
     * @param fromNamespace namespace of the referencing HTTPRoute
     * @param toGroup API group of the referenced resource
     * @param toKind kind of the referenced resource
     * @param toName name of the referenced resource, null for unrestricted grants
     * @return true if a matching grant exists
     */
    public static boolean isGranted(final Lister<DynamicKubernetesObject> grantLister, final String grantNamespace,
                                    final String fromNamespace, final String toGroup, final String toKind,
                                    final String toName) {
        List<DynamicKubernetesObject> grants = grantLister.namespace(grantNamespace).list();
        if (CollectionUtils.isEmpty(grants)) {
            return false;
        }
        for (DynamicKubernetesObject grant : grants) {
            if (matches(grant, fromNamespace, toGroup, toKind, toName)) {
                return true;
            }
        }
        return false;
    }

    /** Requires a matching spec.from (HTTPRoute@fromNamespace) AND spec.to (group/kind, optionally name). */
    private static boolean matches(final DynamicKubernetesObject grant, final String fromNamespace,
                                   final String toGroup, final String toKind, final String toName) {
        JsonObject spec = grant.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec)) {
            return false;
        }
        boolean fromMatched = false;
        JsonArray from = spec.has("from") ? spec.getAsJsonArray("from") : null;
        if (Objects.nonNull(from)) {
            for (JsonElement el : from) {
                JsonObject fromRule = el.getAsJsonObject();
                if (GatewayApiConstants.GATEWAY_API_GROUP.equals(JsonFields.getString(fromRule, "group"))
                        && GatewayApiConstants.HTTP_ROUTE_KIND.equals(JsonFields.getString(fromRule, "kind"))
                        && fromNamespace.equals(JsonFields.getString(fromRule, "namespace"))) {
                    fromMatched = true;
                    break;
                }
            }
        }
        if (!fromMatched) {
            return false;
        }
        JsonArray to = spec.has("to") ? spec.getAsJsonArray("to") : null;
        if (Objects.isNull(to)) {
            return false;
        }
        for (JsonElement el : to) {
            JsonObject toRule = el.getAsJsonObject();
            if (!toGroup.equals(JsonFields.getString(toRule, "group"))
                    || !toKind.equals(JsonFields.getString(toRule, "kind"))) {
                continue;
            }
            String grantedName = JsonFields.getString(toRule, "name");
            if (Objects.isNull(grantedName) || grantedName.equals(toName)) {
                return true;
            }
        }
        return false;
    }
}
