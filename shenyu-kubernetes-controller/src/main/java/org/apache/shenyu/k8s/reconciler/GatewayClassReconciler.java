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

package org.apache.shenyu.k8s.reconciler;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.kubernetes.client.extended.controller.reconciler.Reconciler;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.GatewayApiCrdVersions;
import org.apache.shenyu.k8s.common.JsonFields;
import org.apache.shenyu.k8s.common.StatusMergePatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Accepts GatewayClasses whose controllerName matches ShenYu's; reports the
 * SupportedVersion condition and supportedFeatures, keeps the spec-defined finalizer while
 * Gateways use the class, rejects parametersRef classes (ShenYu has no class-level
 * parameters) and re-queues served Gateways on deletion or ownership loss.
 */
public class GatewayClassReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayClassReconciler.class);

    private static final String GATEWAY_CLASS_KIND = "GatewayClass";

    private static final String GATEWAYCLASSES_RESOURCE = "gatewayclasses";

    /** Official conformance FeatureName strings; sorted ascending (spec) and limited to implemented capabilities. */
    private static final List<String> SUPPORTED_FEATURES = List.of("Gateway", "HTTPRoute", "ReferenceGrant");

    private final Lister<DynamicKubernetesObject> gatewayClassLister;

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final RateLimitingQueue<Request> gatewayWorkQueue;

    private final ApiClient apiClient;

    private final GatewayApiCrdVersions crdVersions;

    public GatewayClassReconciler(final SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer,
                                  final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                                  final RateLimitingQueue<Request> gatewayWorkQueue,
                                  final ApiClient apiClient,
                                  final GatewayApiCrdVersions crdVersions) {
        this.gatewayClassLister = new Lister<>(gatewayClassInformer.getIndexer());
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
        this.gatewayWorkQueue = gatewayWorkQueue;
        this.apiClient = apiClient;
        this.crdVersions = crdVersions;
    }

    @Override
    public Result reconcile(final Request request) {
        LOG.info("Starting to reconcile GatewayClass {}", request.getName());
        try {
            DynamicKubernetesObject gatewayClass = gatewayClassLister.get(request.getName());

            if (Objects.isNull(gatewayClass)) {
                LOG.info("GatewayClass {} deleted, re-queuing affected Gateways", request.getName());
                requeueAffectedGateways(request.getName());
                return new Result(false);
            }

            if (isDeleting(gatewayClass)) {
                // No status writes on a deleting object; drain Gateways, then release the finalizer.
                requeueAffectedGateways(request.getName());
                reconcileFinalizer(gatewayClass, false);
                return new Result(false);
            }

            if (!isShenyuGatewayClass(gatewayClass)) {
                boolean wasAcceptedByShenyu = GatewayApiConstants.isConditionAcceptedByShenyu(gatewayClass, "Accepted");
                boolean anyGatewayRequeued = requeuePreviouslyServedGateways(request.getName());
                if (wasAcceptedByShenyu || anyGatewayRequeued) {
                    LOG.info("GatewayClass {} is no longer managed by ShenYu, re-queuing affected Gateways", request.getName());
                }
                if (wasAcceptedByShenyu) {
                    updateGatewayClassRejectedStatus(gatewayClass, GatewayApiConstants.REASON_UNSUPPORTED,
                            "GatewayClass is not managed by the ShenYu controller");
                }
                // Our finalizer must not outlive the last Gateway once another controller owns the class.
                reconcileFinalizer(gatewayClass, false);
                return new Result(false);
            }

            if (hasParametersRef(gatewayClass)) {
                // Reject instead of silently ignoring class-level parameters we cannot honor.
                boolean wasAcceptedByShenyu = GatewayApiConstants.isConditionAcceptedByShenyu(gatewayClass, "Accepted");
                updateGatewayClassRejectedStatus(gatewayClass, GatewayApiConstants.REASON_INVALID_PARAMETERS,
                        "GatewayClass parametersRef is not supported by the ShenYu controller");
                if (wasAcceptedByShenyu) {
                    requeueAffectedGateways(request.getName());
                }
                reconcileFinalizer(gatewayClass, false);
                return new Result(false);
            }

            // Requeue only on the Accepted transition; plain resyncs would waste a cluster scan.
            boolean wasAccepted = GatewayApiConstants.isConditionTrue(gatewayClass, "Accepted");
            updateGatewayClassAcceptedStatus(gatewayClass);
            if (!wasAccepted) {
                requeueAffectedGateways(request.getName());
            }
            reconcileFinalizer(gatewayClass, true);
            LOG.debug("GatewayClass {} reconciled successfully", request.getName());
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling GatewayClass {}, will retry", request.getName(), e);
            return new Result(true);
        }
    }

    /** Whether the class's controllerName matches ShenYu's. */
    public static boolean isShenyuGatewayClass(final DynamicKubernetesObject gatewayClass) {
        if (Objects.isNull(gatewayClass)) {
            return false;
        }
        JsonObject spec = gatewayClass.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec) || !spec.has("controllerName") || spec.get("controllerName").isJsonNull()) {
            return false;
        }
        String controllerName = spec.get("controllerName").getAsString();
        return GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(controllerName);
    }

    /**
     * Whether the Gateway's class is ShenYu-owned and not rejected; shared by the Gateway
     * and HTTPRoute reconcilers. Accepted=False puts the Gateway out of scope; an absent
     * Accepted is treated optimistically for the startup race (Gateway reconciled before
     * the class status patch reaches the cache) and enforced by the periodic resync.
     */
    public static boolean isShenyuGateway(final DynamicKubernetesObject gateway,
                                          final Lister<DynamicKubernetesObject> gatewayClassLister) {
        if (Objects.isNull(gateway)) {
            return false;
        }
        JsonObject spec = gateway.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec) || !spec.has("gatewayClassName") || spec.get("gatewayClassName").isJsonNull()) {
            return false;
        }
        String gatewayClassName = spec.get("gatewayClassName").getAsString();
        DynamicKubernetesObject gatewayClass = gatewayClassLister.get(gatewayClassName);
        if (!isShenyuGatewayClass(gatewayClass)) {
            return false;
        }
        JsonObject rejected = GatewayApiConstants.findCondition(gatewayClass, GatewayApiConstants.CONDITION_ACCEPTED);
        return Objects.isNull(rejected) || !"False".equals(JsonFields.getString(rejected, "status"));
    }

    /** Whether the class carries a spec.parametersRef ShenYu cannot honor. */
    private static boolean hasParametersRef(final DynamicKubernetesObject gatewayClass) {
        JsonObject spec = JsonFields.getJsonObject(gatewayClass.getRaw(), "spec");
        JsonObject parametersRef = JsonFields.getJsonObject(spec, "parametersRef");
        return Objects.nonNull(parametersRef);
    }

    private static boolean isDeleting(final DynamicKubernetesObject gatewayClass) {
        JsonObject metadata = JsonFields.getJsonObject(gatewayClass.getRaw(), "metadata");
        return Objects.nonNull(JsonFields.getString(metadata, "deletionTimestamp"));
    }

    /** Re-queue Gateways referencing this class (Accepted transition, deletion cascade). */
    private void requeueAffectedGateways(final String gatewayClassName) {
        for (DynamicKubernetesObject gateway : gatewayLister.list()) {
            if (referencesGatewayClass(gateway, gatewayClassName)) {
                String ns = Objects.requireNonNull(gateway.getMetadata()).getNamespace();
                String name = gateway.getMetadata().getName();
                gatewayWorkQueue.add(new Request(ns, name));
                LOG.info("Re-queued Gateway {}/{} due to GatewayClass {} change", ns, name, gatewayClassName);
            }
        }
    }

    /** Re-queue only previously served Gateways on ownership loss; the rest belong to the new controller. */
    private boolean requeuePreviouslyServedGateways(final String gatewayClassName) {
        boolean anyRequeued = false;
        for (DynamicKubernetesObject gateway : gatewayLister.list()) {
            if (!referencesGatewayClass(gateway, gatewayClassName)) {
                continue;
            }
            String ns = Objects.requireNonNull(gateway.getMetadata()).getNamespace();
            String name = gateway.getMetadata().getName();
            boolean servedByShenyu = CollectionUtils.isNotEmpty(GatewayRouteCache.getInstance().getRoutesByGateway(ns, name))
                    || GatewayApiConstants.isConditionAcceptedByShenyu(gateway, GatewayApiConstants.CONDITION_ACCEPTED);
            if (!servedByShenyu) {
                continue;
            }
            gatewayWorkQueue.add(new Request(ns, name));
            LOG.info("Re-queued Gateway {}/{} after GatewayClass {} ownership loss", ns, name, gatewayClassName);
            anyRequeued = true;
        }
        return anyRequeued;
    }

    private boolean referencesGatewayClass(final DynamicKubernetesObject gateway, final String gatewayClassName) {
        JsonObject spec = gateway.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec) || !spec.has("gatewayClassName") || spec.get("gatewayClassName").isJsonNull()) {
            return false;
        }
        return gatewayClassName.equals(spec.get("gatewayClassName").getAsString());
    }

    /** Finalizer present while an accepted class has Gateways, removed once none remain (also releases a deleting class). */
    private void reconcileFinalizer(final DynamicKubernetesObject gatewayClass, final boolean acceptedByShenyu) {
        final String name = gatewayClass.getMetadata().getName();
        JsonArray existing = finalizersOf(gatewayClass);
        boolean hasOurs = containsFinalizer(existing);
        boolean anyGateway = anyGatewayReferences(name);
        try {
            if (anyGateway && !hasOurs && acceptedByShenyu) {
                JsonArray desired = new JsonArray();
                for (JsonElement element : existing) {
                    desired.add(element);
                }
                desired.add(GatewayApiConstants.GATEWAY_CLASS_FINALIZER);
                patchGatewayClassFinalizers(name, desired);
                LOG.info("Added finalizer to GatewayClass {} in use", name);
            } else if (!anyGateway && hasOurs) {
                JsonArray desired = new JsonArray();
                for (JsonElement element : existing) {
                    if (!GatewayApiConstants.GATEWAY_CLASS_FINALIZER.equals(getAsStringOrNull(element))) {
                        desired.add(element);
                    }
                }
                patchGatewayClassFinalizers(name, desired);
                LOG.info("Removed finalizer from GatewayClass {} no longer in use", name);
            }
        } catch (Exception e) {
            LOG.warn("Failed to reconcile GatewayClass {} finalizer, will retry on next resync", name, e);
        }
    }

    private static JsonArray finalizersOf(final DynamicKubernetesObject gatewayClass) {
        JsonObject metadata = JsonFields.getJsonObject(gatewayClass.getRaw(), "metadata");
        JsonArray finalizers = JsonFields.getJsonArray(metadata, "finalizers");
        return Objects.isNull(finalizers) ? new JsonArray() : finalizers;
    }

    private static boolean containsFinalizer(final JsonArray finalizers) {
        for (JsonElement element : finalizers) {
            if (GatewayApiConstants.GATEWAY_CLASS_FINALIZER.equals(getAsStringOrNull(element))) {
                return true;
            }
        }
        return false;
    }

    private static String getAsStringOrNull(final JsonElement element) {
        return Objects.nonNull(element) && element.isJsonPrimitive() ? element.getAsString() : null;
    }

    private boolean anyGatewayReferences(final String gatewayClassName) {
        for (DynamicKubernetesObject gateway : gatewayLister.list()) {
            if (referencesGatewayClass(gateway, gatewayClassName)) {
                return true;
            }
        }
        return false;
    }

    /** Merge-patch the main resource (not the /status subresource) to set metadata.finalizers. */
    private void patchGatewayClassFinalizers(final String name, final JsonArray finalizers) throws ApiException {
        JsonObject body = new JsonObject();
        body.addProperty("kind", GATEWAY_CLASS_KIND);
        body.addProperty("apiVersion", GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION);

        JsonObject metadata = new JsonObject();
        metadata.addProperty("name", name);
        metadata.add("finalizers", finalizers);
        body.add("metadata", metadata);

        String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                + "/" + GATEWAYCLASSES_RESOURCE + "/" + name;

        StatusMergePatch.patch(apiClient, path, body);
    }

    /** Patch Accepted=True + SupportedVersion + supportedFeatures; skipped while the existing status already matches at the current generation. */
    private void updateGatewayClassAcceptedStatus(final DynamicKubernetesObject gatewayClass) {
        Long generation = JsonFields.getLong(JsonFields.getJsonObject(gatewayClass.getRaw(), "metadata"), "generation");
        if (acceptedStatusUpToDate(gatewayClass, generation)) {
            return;
        }
        try {
            final String name = gatewayClass.getMetadata().getName();

            JsonObject accepted = new JsonObject();
            accepted.addProperty("type", GatewayApiConstants.CONDITION_ACCEPTED);
            accepted.addProperty("status", "True");
            accepted.addProperty("reason", GatewayApiConstants.CONDITION_ACCEPTED);
            accepted.addProperty("message", "GatewayClass has been accepted by the ShenYu controller");
            if (Objects.nonNull(generation)) {
                accepted.addProperty("observedGeneration", generation);
            }
            accepted.addProperty("lastTransitionTime", Instant.now().toString());
            preserveTransitionTime(GatewayApiConstants.findCondition(gatewayClass, GatewayApiConstants.CONDITION_ACCEPTED), accepted);

            boolean supported = crdVersions.isSupported();
            JsonObject supportedVersion = new JsonObject();
            supportedVersion.addProperty("type", GatewayApiConstants.CONDITION_SUPPORTED_VERSION);
            supportedVersion.addProperty("status", supported ? "True" : "False");
            supportedVersion.addProperty("reason", supported
                    ? GatewayApiConstants.REASON_SUPPORTED_VERSION : GatewayApiConstants.REASON_UNSUPPORTED_VERSION);
            supportedVersion.addProperty("message", "detected Gateway API CRD bundle version(s): "
                    + crdVersions.describeDetected() + "; supported: >= v1.5.0");
            if (Objects.nonNull(generation)) {
                supportedVersion.addProperty("observedGeneration", generation);
            }
            supportedVersion.addProperty("lastTransitionTime", Instant.now().toString());
            preserveTransitionTime(GatewayApiConstants.findCondition(gatewayClass, GatewayApiConstants.CONDITION_SUPPORTED_VERSION), supportedVersion);

            JsonArray conditions = buildGatewayClassStatusConditions(gatewayClass, accepted, supportedVersion);

            JsonObject statusObj = new JsonObject();
            statusObj.add("conditions", conditions);
            statusObj.add("supportedFeatures", buildSupportedFeatures());

            patchGatewayClassStatus(name, statusObj);
            LOG.info("Updated GatewayClass {} status to Accepted=True", name);
        } catch (Exception e) {
            LOG.warn("Failed to update GatewayClass status, will retry on next resync", e);
        }
    }

    /** Accepted=False with the given reason; skipped when already saying the same at the current generation. */
    private void updateGatewayClassRejectedStatus(final DynamicKubernetesObject gatewayClass, final String reason,
                                                  final String message) {
        Long generation = JsonFields.getLong(JsonFields.getJsonObject(gatewayClass.getRaw(), "metadata"), "generation");
        JsonObject existing = GatewayApiConstants.findCondition(gatewayClass, GatewayApiConstants.CONDITION_ACCEPTED);
        if (Objects.nonNull(existing)
                && "False".equals(JsonFields.getString(existing, "status"))
                && reason.equals(JsonFields.getString(existing, "reason"))
                && observedGenerationUpToDate(existing, generation)) {
            return;
        }
        try {
            final String name = gatewayClass.getMetadata().getName();

            JsonObject condition = new JsonObject();
            condition.addProperty("type", GatewayApiConstants.CONDITION_ACCEPTED);
            condition.addProperty("status", "False");
            condition.addProperty("reason", reason);
            condition.addProperty("message", message);
            if (Objects.nonNull(generation)) {
                condition.addProperty("observedGeneration", generation);
            }
            condition.addProperty("lastTransitionTime", Instant.now().toString());

            JsonArray conditions = buildGatewayClassStatusConditions(gatewayClass, condition);

            JsonObject statusObj = new JsonObject();
            statusObj.add("conditions", conditions);

            patchGatewayClassStatus(name, statusObj);
            LOG.info("Updated GatewayClass {} status to Accepted=False ({})", name, reason);
        } catch (Exception e) {
            LOG.warn("Failed to update GatewayClass status, will retry on next resync", e);
        }
    }

    private void patchGatewayClassStatus(final String name, final JsonObject statusObj) throws ApiException {
        JsonObject body = new JsonObject();
        body.add("status", statusObj);
        body.addProperty("kind", GATEWAY_CLASS_KIND);
        body.addProperty("apiVersion", GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION);

        JsonObject metadata = new JsonObject();
        metadata.addProperty("name", name);
        body.add("metadata", metadata);

        String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                + "/" + GATEWAYCLASSES_RESOURCE + "/" + name + "/status";

        StatusMergePatch.patch(apiClient, path, body);
    }

    private boolean acceptedStatusUpToDate(final DynamicKubernetesObject gatewayClass, final Long generation) {
        JsonObject existingAccepted = GatewayApiConstants.findCondition(gatewayClass, GatewayApiConstants.CONDITION_ACCEPTED);
        if (!GatewayApiConstants.isConditionTrue(gatewayClass, GatewayApiConstants.CONDITION_ACCEPTED)
                || !observedGenerationUpToDate(existingAccepted, generation)) {
            return false;
        }
        JsonObject existingVersion = GatewayApiConstants.findCondition(gatewayClass, GatewayApiConstants.CONDITION_SUPPORTED_VERSION);
        boolean supported = crdVersions.isSupported();
        if (Objects.isNull(existingVersion)
                || !(supported ? "True" : "False").equals(JsonFields.getString(existingVersion, "status"))
                || !supportedVersionReason(supported).equals(JsonFields.getString(existingVersion, "reason"))) {
            return false;
        }
        return supportedFeaturesMatch(gatewayClass);
    }

    private String supportedVersionReason(final boolean supported) {
        return supported ? GatewayApiConstants.REASON_SUPPORTED_VERSION : GatewayApiConstants.REASON_UNSUPPORTED_VERSION;
    }

    private boolean supportedFeaturesMatch(final DynamicKubernetesObject gatewayClass) {
        JsonObject status = JsonFields.getJsonObject(gatewayClass.getRaw(), "status");
        JsonArray existing = JsonFields.getJsonArray(status, "supportedFeatures");
        if (Objects.isNull(existing) || existing.size() != SUPPORTED_FEATURES.size()) {
            return false;
        }
        List<String> names = new ArrayList<>();
        for (JsonElement element : existing) {
            JsonObject feature = element.isJsonObject() ? element.getAsJsonObject() : null;
            String name = Objects.isNull(feature) ? null : JsonFields.getString(feature, "name");
            if (Objects.isNull(name)) {
                return false;
            }
            names.add(name);
        }
        return names.equals(SUPPORTED_FEATURES);
    }

    private static JsonArray buildSupportedFeatures() {
        JsonArray features = new JsonArray();
        for (String name : SUPPORTED_FEATURES) {
            JsonObject feature = new JsonObject();
            feature.addProperty("name", name);
            features.add(feature);
        }
        return features;
    }

    private boolean observedGenerationUpToDate(final JsonObject existingCondition, final Long generation) {
        if (Objects.isNull(generation)) {
            return true;
        }
        return Objects.nonNull(existingCondition)
                && generation.equals(JsonFields.getLong(existingCondition, "observedGeneration"));
    }

    /** Keep lastTransitionTime when type and status are unchanged; a generation refresh is not a transition. */
    private void preserveTransitionTime(final JsonObject existingCondition, final JsonObject desiredCondition) {
        if (Objects.isNull(existingCondition)
                || !Objects.equals(JsonFields.getString(existingCondition, "type"), JsonFields.getString(desiredCondition, "type"))
                || !Objects.equals(JsonFields.getString(existingCondition, "status"), JsonFields.getString(desiredCondition, "status"))) {
            return;
        }
        String existingTime = JsonFields.getString(existingCondition, "lastTransitionTime");
        if (Objects.nonNull(existingTime)) {
            desiredCondition.addProperty("lastTransitionTime", existingTime);
        }
    }

    /** Own conditions plus foreign ones: merge-patch replaces arrays wholesale. */
    private JsonArray buildGatewayClassStatusConditions(final DynamicKubernetesObject gatewayClass,
                                                        final JsonObject... ownConditions) {
        Set<String> ownTypes = new HashSet<>();
        JsonArray conditions = new JsonArray();
        for (JsonObject ownCondition : ownConditions) {
            conditions.add(ownCondition);
            ownTypes.add(JsonFields.getString(ownCondition, "type"));
        }

        JsonObject raw = gatewayClass.getRaw();
        if (raw.has("status") && !raw.get("status").isJsonNull()) {
            JsonObject status = raw.getAsJsonObject("status");
            if (status.has("conditions") && !status.get("conditions").isJsonNull()) {
                for (JsonElement el : status.getAsJsonArray("conditions")) {
                    JsonObject existing = el.getAsJsonObject();
                    String existingType = existing.has("type") ? existing.get("type").getAsString() : null;
                    // Drop our stale entries of the same types; keep everything else.
                    if (!ownTypes.contains(existingType)) {
                        conditions.add(existing);
                    }
                }
            }
        }
        return conditions;
    }
}
