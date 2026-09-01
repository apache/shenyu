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
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.apache.shenyu.k8s.common.JsonFields;
import org.apache.shenyu.k8s.common.StatusMergePatch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.Objects;

/**
 * Reconciler for the cluster-scoped GatewayClass resources: accepts classes whose
 * spec.controllerName matches ShenYu's controller name (Accepted=True status), and on
 * deletion or ownership loss (controllerName re-pointed away from ShenYu) re-queues the
 * Gateways previously served through the class for cascade cleanup.
 */
public class GatewayClassReconciler implements Reconciler {

    private static final Logger LOG = LoggerFactory.getLogger(GatewayClassReconciler.class);

    private static final String GATEWAY_CLASS_KIND = "GatewayClass";

    private static final String GATEWAYCLASSES_RESOURCE = "gatewayclasses";

    private final Lister<DynamicKubernetesObject> gatewayClassLister;

    private final Lister<DynamicKubernetesObject> gatewayLister;

    private final RateLimitingQueue<Request> gatewayWorkQueue;

    private final ApiClient apiClient;

    public GatewayClassReconciler(final SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer,
                                  final SharedIndexInformer<DynamicKubernetesObject> gatewayInformer,
                                  final RateLimitingQueue<Request> gatewayWorkQueue,
                                  final ApiClient apiClient) {
        this.gatewayClassLister = new Lister<>(gatewayClassInformer.getIndexer());
        this.gatewayLister = new Lister<>(gatewayInformer.getIndexer());
        this.gatewayWorkQueue = gatewayWorkQueue;
        this.apiClient = apiClient;
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

            if (!isShenyuGatewayClass(gatewayClass)) {
                boolean wasAcceptedByShenyu = GatewayApiConstants.isConditionAcceptedByShenyu(gatewayClass, "Accepted");
                boolean anyGatewayRequeued = requeuePreviouslyServedGateways(request.getName());
                if (wasAcceptedByShenyu || anyGatewayRequeued) {
                    LOG.info("GatewayClass {} is no longer managed by ShenYu, re-queuing affected Gateways", request.getName());
                }
                if (wasAcceptedByShenyu) {
                    updateGatewayClassNotAcceptedStatus(gatewayClass);
                }
                return new Result(false);
            }

            // Requeue only on the Accepted transition (first accept or after class restore):
            // on plain resyncs Gateways are already reconciled and a cluster scan is wasted.
            boolean wasAccepted = GatewayApiConstants.isConditionTrue(gatewayClass, "Accepted");
            updateGatewayClassAcceptedStatus(gatewayClass);
            if (!wasAccepted) {
                requeueAffectedGateways(request.getName());
            }
            LOG.debug("GatewayClass {} reconciled successfully", request.getName());
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling GatewayClass {}, will retry", request.getName(), e);
            return new Result(true);
        }
    }

    /**
     * Check if the GatewayClass is managed by ShenYu by comparing spec.controllerName.
     *
     * @param gatewayClass the GatewayClass dynamic object
     * @return true if the GatewayClass's controllerName matches ShenYu's controller name
     */
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
     * Check if a Gateway is ShenYu-managed by resolving its GatewayClass's
     * {@code spec.controllerName}, so Gateways referencing a ShenYu-owned class under any
     * name are accepted. Shared by the Gateway and HTTPRoute reconcilers.
     *
     * @param gateway the Gateway dynamic object
     * @param gatewayClassLister lister for GatewayClass (cluster-scoped)
     * @return true if the Gateway's class is owned by ShenYu
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
        return isShenyuGatewayClass(gatewayClassLister.get(gatewayClassName));
    }

    /**
     * Re-queue Gateways referencing this class: on the Accepted transition (restores routes
     * after a class is accepted or recreated) and on deletion (cascade cleanup).
     */
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

    /**
     * Re-queue only the Gateways referencing this class that ShenYu previously served,
     * detected by live route bindings or by ShenYu's own Accepted status payload. Used on
     * the ownership-loss transition: other Gateways of the (now foreign) class belong to
     * its new controller and must not be touched.
     *
     * @param gatewayClassName name of the GatewayClass
     * @return whether any Gateway was re-queued
     */
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

    /**
     * Update GatewayClass status with Accepted=True condition.
     * GatewayClass is cluster-scoped, so the API path has no namespace segment.
     *
     * <p>Skipped only when the existing Accepted=True condition already carries the current
     * metadata generation: returning on any Accepted=True would leave its
     * observedGeneration stale after a spec change. lastTransitionTime is preserved for an
     * unchanged condition, as the spec requires it to advance only on a status transition.
     */
    private void updateGatewayClassAcceptedStatus(final DynamicKubernetesObject gatewayClass) {
        Long generation = JsonFields.getLong(JsonFields.getJsonObject(gatewayClass.getRaw(), "metadata"), "generation");
        JsonObject existingAccepted = GatewayApiConstants.findCondition(gatewayClass, "Accepted");
        if (GatewayApiConstants.isConditionTrue(gatewayClass, "Accepted")
                && observedGenerationUpToDate(existingAccepted, generation)) {
            return;
        }
        try {
            final String name = gatewayClass.getMetadata().getName();

            JsonObject condition = new JsonObject();
            condition.addProperty("type", "Accepted");
            condition.addProperty("status", "True");
            condition.addProperty("reason", "Accepted");
            condition.addProperty("message", "GatewayClass has been accepted by the ShenYu controller");
            if (Objects.nonNull(generation)) {
                condition.addProperty("observedGeneration", generation);
            }
            condition.addProperty("lastTransitionTime", Instant.now().toString());
            preserveTransitionTime(existingAccepted, condition);

            JsonArray conditions = buildGatewayClassStatusConditions(gatewayClass, condition);

            JsonObject statusObj = new JsonObject();
            statusObj.add("conditions", conditions);

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
            LOG.info("Updated GatewayClass {} status to Accepted=True", name);
        } catch (Exception e) {
            LOG.warn("Failed to update GatewayClass status, will retry on next resync", e);
        }
    }

    /**
     * Clear ShenYu's Accepted entry on a GatewayClass this controller no longer owns, so
     * the class does not advertise ShenYu acceptance and a later restore re-triggers the
     * Accepted transition.
     */
    private void updateGatewayClassNotAcceptedStatus(final DynamicKubernetesObject gatewayClass) {
        try {
            final String name = gatewayClass.getMetadata().getName();

            JsonObject condition = new JsonObject();
            condition.addProperty("type", "Accepted");
            condition.addProperty("status", "False");
            condition.addProperty("reason", "NoGatewayClassController");
            condition.addProperty("message", "GatewayClass is not managed by the ShenYu controller");
            Long generation = JsonFields.getLong(JsonFields.getJsonObject(gatewayClass.getRaw(), "metadata"), "generation");
            if (Objects.nonNull(generation)) {
                condition.addProperty("observedGeneration", generation);
            }
            condition.addProperty("lastTransitionTime", Instant.now().toString());

            JsonArray conditions = buildGatewayClassStatusConditions(gatewayClass, condition);

            JsonObject statusObj = new JsonObject();
            statusObj.add("conditions", conditions);

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
            LOG.info("Updated GatewayClass {} status to Accepted=False after ownership loss", name);
        } catch (Exception e) {
            LOG.warn("Failed to downgrade GatewayClass status, will retry on next resync", e);
        }
    }

    private boolean observedGenerationUpToDate(final JsonObject existingCondition, final Long generation) {
        if (Objects.isNull(generation)) {
            return true;
        }
        return Objects.nonNull(existingCondition)
                && generation.equals(JsonFields.getLong(existingCondition, "observedGeneration"));
    }

    /**
     * Carry over the lastTransitionTime of an existing Accepted=True condition: a refresh
     * of observedGeneration alone is not a status transition and must not move the
     * timestamp.
     */
    private void preserveTransitionTime(final JsonObject existingCondition, final JsonObject desiredCondition) {
        if (Objects.isNull(existingCondition)
                || !"True".equals(JsonFields.getString(existingCondition, "status"))
                || !"True".equals(JsonFields.getString(desiredCondition, "status"))) {
            return;
        }
        String existingTime = JsonFields.getString(existingCondition, "lastTransitionTime");
        if (Objects.nonNull(existingTime)) {
            desiredCondition.addProperty("lastTransitionTime", existingTime);
        }
    }

    /**
     * Build the patch-body conditions array: the Accepted condition plus all existing
     * non-Accepted conditions, so merge-patch (which replaces arrays wholesale) does not
     * clobber conditions owned by other controllers.
     */
    private JsonArray buildGatewayClassStatusConditions(final DynamicKubernetesObject gatewayClass,
                                                        final JsonObject acceptedCondition) {
        JsonArray conditions = new JsonArray();
        conditions.add(acceptedCondition);

        JsonObject raw = gatewayClass.getRaw();
        if (raw.has("status") && !raw.get("status").isJsonNull()) {
            JsonObject status = raw.getAsJsonObject("status");
            if (status.has("conditions") && !status.get("conditions").isJsonNull()) {
                for (JsonElement el : status.getAsJsonArray("conditions")) {
                    JsonObject existing = el.getAsJsonObject();
                    String existingType = existing.has("type") ? existing.get("type").getAsString() : null;
                    // Drop any stale Accepted entry from other controllers; keep everything else.
                    if (!"Accepted".equals(existingType)) {
                        conditions.add(existing);
                    }
                }
            }
        }
        return conditions;
    }
}
