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

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.kubernetes.client.extended.controller.reconciler.Reconciler;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.common.GatewayApiConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.Objects;

/**
 * Reconciler for GatewayClass resources.
 * GatewayClass is a cluster-scoped resource that defines which controller manages gateways of this class.
 * This reconciler watches GatewayClass objects and:
 * <ul>
 *   <li>Accepts GatewayClasses whose spec.controllerName matches ShenYu's controller name</li>
 *   <li>Updates GatewayClass status with Accepted=True condition</li>
 *   <li>On deletion, re-queues associated Gateways for cleanup</li>
 * </ul>
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
            // GatewayClass is cluster-scoped, no namespace
            DynamicKubernetesObject gatewayClass = gatewayClassLister.get(request.getName());

            if (Objects.isNull(gatewayClass)) {
                LOG.info("GatewayClass {} deleted, re-queuing affected Gateways", request.getName());
                requeueAffectedGateways(request.getName());
                return new Result(false);
            }

            if (!isShenyuGatewayClass(gatewayClass)) {
                LOG.info("GatewayClass {} is not managed by ShenYu, skipping", request.getName());
                return new Result(false);
            }

            updateGatewayClassAcceptedStatus(gatewayClass);
            LOG.info("GatewayClass {} reconciled successfully", request.getName());
            return new Result(false);
        } catch (Exception e) {
            LOG.error("Error reconciling GatewayClass {}, will retry", request.getName(), e);
            return new Result(true);
        }
    }

    /**
     * Check if the given GatewayClass is managed by ShenYu by comparing spec.controllerName.
     *
     * @param gatewayClass the GatewayClass dynamic object
     * @return true if the GatewayClass's controllerName matches ShenYu's controller name
     */
    public static boolean isShenyuGatewayClass(final DynamicKubernetesObject gatewayClass) {
        JsonObject spec = gatewayClass.getRaw().getAsJsonObject("spec");
        if (Objects.isNull(spec) || !spec.has("controllerName") || spec.get("controllerName").isJsonNull()) {
            return false;
        }
        String controllerName = spec.get("controllerName").getAsString();
        return GatewayApiConstants.SHENYU_CONTROLLER_NAME.equals(controllerName);
    }

    /**
     * When a ShenYu GatewayClass is deleted, find all Gateways that reference this class
     * and re-queue them for reconciliation (which will handle cleanup).
     */
    private void requeueAffectedGateways(final String gatewayClassName) {
        for (DynamicKubernetesObject gateway : gatewayLister.list()) {
            if (referencesGatewayClass(gateway, gatewayClassName)) {
                String ns = Objects.requireNonNull(gateway.getMetadata()).getNamespace();
                String name = gateway.getMetadata().getName();
                gatewayWorkQueue.add(new Request(ns, name));
                LOG.info("Re-queued Gateway {}/{} due to GatewayClass {} deletion", ns, name, gatewayClassName);
            }
        }
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
     */
    private void updateGatewayClassAcceptedStatus(final DynamicKubernetesObject gatewayClass) {
        if (GatewayApiConstants.isConditionTrue(gatewayClass, "Accepted")) {
            return;
        }
        try {
            final String name = gatewayClass.getMetadata().getName();

            JsonObject condition = new JsonObject();
            condition.addProperty("type", "Accepted");
            condition.addProperty("status", "True");
            condition.addProperty("reason", "Accepted");
            condition.addProperty("message", "GatewayClass has been accepted by the ShenYu controller");
            condition.addProperty("lastTransitionTime", Instant.now().toString());

            JsonArray conditions = new JsonArray();
            conditions.add(condition);

            JsonObject statusObj = new JsonObject();
            statusObj.add("conditions", conditions);

            JsonObject body = new JsonObject();
            body.add("status", statusObj);
            body.addProperty("kind", GATEWAY_CLASS_KIND);
            body.addProperty("apiVersion", GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION);

            JsonObject metadata = new JsonObject();
            metadata.addProperty("name", name);
            body.add("metadata", metadata);

            String patchBody = new Gson().toJson(body);
            String path = "/apis/" + GatewayApiConstants.GATEWAY_API_GROUP + "/" + GatewayApiConstants.GATEWAY_API_VERSION
                    + "/" + GATEWAYCLASSES_RESOURCE + "/" + name + "/status";

            okhttp3.Request httpRequest = new okhttp3.Request.Builder()
                    .url(apiClient.getBasePath() + path)
                    .patch(okhttp3.RequestBody.create(patchBody, okhttp3.MediaType.parse("application/merge-patch+json")))
                    .build();

            try (okhttp3.Response response = apiClient.getHttpClient().newCall(httpRequest).execute()) {
                if (response.isSuccessful()) {
                    LOG.info("Updated GatewayClass {} status to Accepted=True", name);
                } else {
                    String responseBody = Objects.nonNull(response.body()) ? response.body().string() : "empty";
                    LOG.warn("Failed to update GatewayClass {} status: {} - {}", name, response.code(), responseBody);
                }
            }
        } catch (Exception e) {
            LOG.warn("Failed to update GatewayClass status, will retry on next resync", e);
        }
    }

}
