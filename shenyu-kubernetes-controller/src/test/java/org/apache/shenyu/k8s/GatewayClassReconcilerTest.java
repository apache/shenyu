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

package org.apache.shenyu.k8s;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.controller.reconciler.Result;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Indexer;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;
import org.apache.shenyu.k8s.cache.GatewayRouteCache;
import org.apache.shenyu.k8s.common.GatewayApiCrdVersions;
import org.apache.shenyu.k8s.reconciler.GatewayClassReconciler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * GatewayClass Reconciler Test: acceptance (with SupportedVersion and supportedFeatures
 * reporting), parametersRef rejection, the spec-defined finalizer lifecycle, foreign
 * classes, and the ownership-loss transition (controllerName re-pointed away from ShenYu)
 * which must re-queue previously served Gateways immediately instead of waiting for their
 * resync.
 */
public final class GatewayClassReconcilerTest {

    private static final String SHENYU_CONTROLLER = "gateway.shenyu.apache.org/shenyu-controller";

    private static final String FINALIZER = "gateway-exists-finalizer.gateway.networking.k8s.io";

    private static final String STATUS_PATH_SUFFIX = "/status";

    private RateLimitingQueue<Request> gatewayWorkQueue;

    private ApiClient reconcilerApi;

    private ArgumentCaptor<String> pathCaptor;

    private ArgumentCaptor<Object> bodyCaptor;

    @BeforeEach
    public void setUp() {
        GatewayRouteCache.getInstance().clear();
    }

    /** A ShenYu GatewayClass without Accepted status gets Accepted=True patched together with the SupportedVersion condition and the supportedFeatures list. */
    @Test
    public void testShenYuGatewayClassGetsAcceptedStatus() throws Exception {
        GatewayClassReconciler reconciler = reconciler(
                gatewayClass("shenyu", SHENYU_CONTROLLER, null), null);

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);
        verify(reconcilerApi).execute(any(okhttp3.Call.class));

        JsonObject status = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status");
        Assertions.assertEquals(2, status.getAsJsonArray("conditions").size());
        JsonObject accepted = condition(status, "Accepted");
        Assertions.assertEquals("True", accepted.get("status").getAsString());
        JsonObject supportedVersion = condition(status, "SupportedVersion");
        Assertions.assertEquals("True", supportedVersion.get("status").getAsString());
        Assertions.assertEquals("SupportedVersion", supportedVersion.get("reason").getAsString());
        Assertions.assertEquals(3, status.getAsJsonArray("supportedFeatures").size());
        Assertions.assertEquals("Gateway", featureName(status, 0));
        Assertions.assertEquals("HTTPRoute", featureName(status, 1));
        Assertions.assertEquals("ReferenceGrant", featureName(status, 2));
    }

    /** Unsupported CRD bundle versions still accept the class (best effort) but flip the SupportedVersion condition to False with the standard reason. */
    @Test
    public void testUnsupportedCrdVersionsFlipSupportedVersion() throws Exception {
        GatewayClassReconciler reconciler = reconciler(
                gatewayClass("shenyu", SHENYU_CONTROLLER, null), null,
                new GatewayApiCrdVersions(List.of("main"), false));

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);

        JsonObject status = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status");
        Assertions.assertEquals("True", condition(status, "Accepted").get("status").getAsString());
        Assertions.assertEquals("False", condition(status, "SupportedVersion").get("status").getAsString());
        Assertions.assertEquals("UnsupportedVersion", condition(status, "SupportedVersion").get("reason").getAsString());
    }

    /** Steady state: an already-reported status (Accepted=True at the current generation, matching SupportedVersion and supportedFeatures) produces no patch at all. */
    @Test
    public void testSteadyStateStatusIsNotPatchedAgain() throws Exception {
        GatewayClassReconciler reconciler = reconciler(
                gatewayClass("shenyu", SHENYU_CONTROLLER, steadyStatus()), null);

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);
        verify(reconcilerApi, never()).execute(any(okhttp3.Call.class));
    }

    /** A class with a parametersRef is rejected with Accepted=False/InvalidParameters and its never-served Gateways are left alone. */
    @Test
    public void testParametersRefClassIsRejected() throws Exception {
        DynamicKubernetesObject gatewayClass = gatewayClass("shenyu", SHENYU_CONTROLLER, null);
        JsonObject parametersRef = new JsonObject();
        parametersRef.addProperty("group", "gateway.shenyu.apache.org");
        parametersRef.addProperty("kind", "ShenyuClassConfig");
        parametersRef.addProperty("name", "cfg");
        gatewayClass.getRaw().getAsJsonObject("spec").add("parametersRef", parametersRef);

        GatewayClassReconciler reconciler = reconciler(gatewayClass, null);

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);
        verify(gatewayWorkQueue, never()).add(any(Request.class));
        verify(reconcilerApi).execute(any(okhttp3.Call.class));

        JsonObject status = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status");
        JsonObject rejected = condition(status, "Accepted");
        Assertions.assertEquals("False", rejected.get("status").getAsString());
        Assertions.assertEquals("InvalidParameters", rejected.get("reason").getAsString());
    }

    /** Ownership loss: the class was accepted by ShenYu (our Accepted=True payload) and its controllerName moved to another controller. */
    @Test
    public void testOwnershipLossRequeuesServedGatewaysAndDowngradesStatus() throws Exception {
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", "True");
        accepted.addProperty("reason", "Accepted");
        accepted.addProperty("message", "GatewayClass has been accepted by the ShenYu controller");
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        JsonObject status = new JsonObject();
        status.add("conditions", conditions);

        GatewayRouteCache cache = GatewayRouteCache.getInstance();
        cache.bindRouteToGateway("mockedNamespace", "shenyu-gateway", Set.of("http"),
                "mockedNamespace", "test-route");

        GatewayClassReconciler reconciler = reconciler(
                gatewayClass("shenyu", "example.com/other-controller", status),
                gateway("mockedNamespace", "shenyu-gateway", "shenyu"));

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);
        verify(gatewayWorkQueue).add(new Request("mockedNamespace", "shenyu-gateway"));
        verify(reconcilerApi).execute(any(okhttp3.Call.class));

        JsonObject patched = ((JsonObject) bodyCaptor.getValue()).getAsJsonObject("status");
        Assertions.assertEquals("False", condition(patched, "Accepted").get("status").getAsString());
        Assertions.assertEquals("Unsupported", condition(patched, "Accepted").get("reason").getAsString());
    }

    /** A foreign class ShenYu never served (no bindings, no ShenYu-written status) must be skipped entirely: re-queuing its Gateways or patching its status would fight the controller that owns it. */
    @Test
    public void testForeignGatewayClassNeverServedIsSkipped() throws Exception {
        GatewayClassReconciler reconciler = reconciler(
                gatewayClass("other-class", "example.com/other-controller", null),
                gateway("mockedNamespace", "some-gateway", "other-class"));

        Result result = reconciler.reconcile(new Request("", "other-class"));
        Assertions.assertEquals(new Result(false), result);
        verify(gatewayWorkQueue, never()).add(any(Request.class));
        verify(reconcilerApi, never()).execute(any(okhttp3.Call.class));
    }

    /** An accepted class used by a Gateway gains the spec-defined finalizer through a patch on the main resource (not the /status subresource). */
    @Test
    public void testFinalizerAddedWhenGatewayUsesClass() throws Exception {
        GatewayClassReconciler reconciler = reconciler(
                gatewayClass("shenyu", SHENYU_CONTROLLER, null),
                gateway("mockedNamespace", "shenyu-gateway", "shenyu"));

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);

        boolean finalizerPatchFound = false;
        for (int i = 0; i < pathCaptor.getAllValues().size(); i++) {
            String path = pathCaptor.getAllValues().get(i);
            if (!path.endsWith(STATUS_PATH_SUFFIX)) {
                finalizerPatchFound = true;
                JsonArray finalizers = ((JsonObject) bodyCaptor.getAllValues().get(i))
                        .getAsJsonObject("metadata").getAsJsonArray("finalizers");
                Assertions.assertEquals(1, finalizers.size());
                Assertions.assertEquals(FINALIZER, finalizers.get(0).getAsString());
            }
        }
        Assertions.assertTrue(finalizerPatchFound, "a finalizer patch on the main resource is expected");
    }

    /** The finalizer is removed once no Gateway references the class anymore. */
    @Test
    public void testFinalizerRemovedWhenNoGatewayUsesClass() throws Exception {
        DynamicKubernetesObject gatewayClass = gatewayClass("shenyu", SHENYU_CONTROLLER, null);
        JsonArray finalizers = new JsonArray();
        finalizers.add(FINALIZER);
        gatewayClass.getRaw().getAsJsonObject("metadata").add("finalizers", finalizers);

        GatewayClassReconciler reconciler = reconciler(gatewayClass, null);

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);

        boolean removalPatchFound = false;
        for (int i = 0; i < pathCaptor.getAllValues().size(); i++) {
            if (!pathCaptor.getAllValues().get(i).endsWith(STATUS_PATH_SUFFIX)) {
                removalPatchFound = true;
                JsonArray patched = ((JsonObject) bodyCaptor.getAllValues().get(i))
                        .getAsJsonObject("metadata").getAsJsonArray("finalizers");
                Assertions.assertEquals(0, patched.size());
            }
        }
        Assertions.assertTrue(removalPatchFound, "a finalizer removal patch is expected");
    }

    /** A deleting class gets no status writes, keeps the finalizer while Gateways still reference it, and is released once they are gone. */
    @Test
    public void testDeletingClassKeepsFinalizerWhileInUseAndReleasesAfterDrain() throws Exception {
        DynamicKubernetesObject inUse = deletingClass();
        GatewayClassReconciler reconciler = reconciler(inUse,
                gateway("mockedNamespace", "shenyu-gateway", "shenyu"));
        Assertions.assertEquals(new Result(false), reconciler.reconcile(new Request("", "shenyu")));
        // no status patch, no finalizer change: the class is still in use
        verify(reconcilerApi, never()).execute(any(okhttp3.Call.class));
        verify(gatewayWorkQueue).add(new Request("mockedNamespace", "shenyu-gateway"));

        DynamicKubernetesObject drained = deletingClass();
        reconciler = reconciler(drained, null);
        Assertions.assertEquals(new Result(false), reconciler.reconcile(new Request("", "shenyu")));
        verify(reconcilerApi, times(1)).execute(any(okhttp3.Call.class));
        Assertions.assertFalse(pathCaptor.getValue().endsWith(STATUS_PATH_SUFFIX));
        JsonArray patched = ((JsonObject) bodyCaptor.getValue())
                .getAsJsonObject("metadata").getAsJsonArray("finalizers");
        Assertions.assertEquals(0, patched.size());
    }

    private DynamicKubernetesObject deletingClass() {
        DynamicKubernetesObject gatewayClass = gatewayClass("shenyu", SHENYU_CONTROLLER, null);
        JsonArray finalizers = new JsonArray();
        finalizers.add(FINALIZER);
        gatewayClass.getRaw().getAsJsonObject("metadata").add("finalizers", finalizers);
        gatewayClass.getRaw().getAsJsonObject("metadata").addProperty("deletionTimestamp", "2026-01-01T00:00:00Z");
        return gatewayClass;
    }

    private static JsonObject condition(final JsonObject status, final String type) {
        JsonArray conditions = status.getAsJsonArray("conditions");
        for (JsonElement element : conditions) {
            if (type.equals(element.getAsJsonObject().get("type").getAsString())) {
                return element.getAsJsonObject();
            }
        }
        throw new AssertionError("condition " + type + " not found");
    }

    private static String featureName(final JsonObject status, final int index) {
        return status.getAsJsonArray("supportedFeatures").get(index).getAsJsonObject()
                .get("name").getAsString();
    }

    /** The full status an accepted, fully reported class carries in steady state. */
    private static JsonObject steadyStatus() {
        JsonObject accepted = new JsonObject();
        accepted.addProperty("type", "Accepted");
        accepted.addProperty("status", "True");
        accepted.addProperty("reason", "Accepted");
        accepted.addProperty("message", "GatewayClass has been accepted by the ShenYu controller");
        accepted.addProperty("observedGeneration", 1L);
        accepted.addProperty("lastTransitionTime", "2026-01-01T00:00:00Z");
        JsonObject supportedVersion = new JsonObject();
        supportedVersion.addProperty("type", "SupportedVersion");
        supportedVersion.addProperty("status", "True");
        supportedVersion.addProperty("reason", "SupportedVersion");
        supportedVersion.addProperty("message", "detected Gateway API CRD bundle version(s): v1.5.1; supported: >= v1.5.0");
        supportedVersion.addProperty("observedGeneration", 1L);
        supportedVersion.addProperty("lastTransitionTime", "2026-01-01T00:00:00Z");
        JsonArray conditions = new JsonArray();
        conditions.add(accepted);
        conditions.add(supportedVersion);

        JsonArray supportedFeatures = new JsonArray();
        for (String name : List.of("Gateway", "HTTPRoute", "ReferenceGrant")) {
            JsonObject feature = new JsonObject();
            feature.addProperty("name", name);
            supportedFeatures.add(feature);
        }

        JsonObject status = new JsonObject();
        status.add("conditions", conditions);
        status.add("supportedFeatures", supportedFeatures);
        return status;
    }

    private GatewayClassReconciler reconciler(final DynamicKubernetesObject gatewayClass,
                                              final DynamicKubernetesObject gateway) {
        return reconciler(gatewayClass, gateway, new GatewayApiCrdVersions(List.of("v1.5.1"), true));
    }

    private GatewayClassReconciler reconciler(final DynamicKubernetesObject gatewayClass,
                                              final DynamicKubernetesObject gateway,
                                              final GatewayApiCrdVersions crdVersions) {
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayClassIndexer = mock(Indexer.class);
        when(gatewayClassIndexer.getByKey("shenyu")).thenReturn(gatewayClass);
        when(gatewayClassIndexer.getByKey("other-class")).thenReturn(gatewayClass);
        when(gatewayClassInformer.getIndexer()).thenReturn(gatewayClassIndexer);

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        if (Objects.nonNull(gateway)) {
            when(gatewayIndexer.list()).thenReturn(List.of(gateway));
        } else {
            when(gatewayIndexer.list()).thenReturn(List.of());
        }
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        gatewayWorkQueue = mock(RateLimitingQueue.class);
        reconcilerApi = mock(ApiClient.class);
        pathCaptor = ArgumentCaptor.forClass(String.class);
        bodyCaptor = ArgumentCaptor.forClass(Object.class);
        try {
            when(reconcilerApi.getAuthentications()).thenReturn(Map.of());
            when(reconcilerApi.buildCall(pathCaptor.capture(), any(), any(), any(), bodyCaptor.capture(),
                    any(), any(), any(), any(), any()))
                    .thenReturn(mock(okhttp3.Call.class));
        } catch (ApiException e) {
            throw new IllegalStateException(e);
        }
        return new GatewayClassReconciler(gatewayClassInformer, gatewayInformer, gatewayWorkQueue, reconcilerApi, crdVersions);
    }

    private DynamicKubernetesObject gatewayClass(final String name, final String controllerName,
                                                 final JsonObject status) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("name", name);
        metadata.addProperty("generation", 1L);

        JsonObject spec = new JsonObject();
        spec.addProperty("controllerName", controllerName);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "GatewayClass");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        if (Objects.nonNull(status)) {
            raw.add("status", status);
        }
        return new DynamicKubernetesObject(raw);
    }

    private DynamicKubernetesObject gateway(final String namespace, final String name,
                                            final String gatewayClassName) {
        JsonObject metadata = new JsonObject();
        metadata.addProperty("namespace", namespace);
        metadata.addProperty("name", name);

        JsonObject listener = new JsonObject();
        listener.addProperty("name", "http");
        listener.addProperty("protocol", "HTTP");
        listener.addProperty("port", 9195);
        JsonArray listeners = new JsonArray();
        listeners.add(listener);

        JsonObject spec = new JsonObject();
        spec.addProperty("gatewayClassName", gatewayClassName);
        spec.add("listeners", listeners);

        JsonObject raw = new JsonObject();
        raw.addProperty("apiVersion", "gateway.networking.k8s.io/v1");
        raw.addProperty("kind", "Gateway");
        raw.add("metadata", metadata);
        raw.add("spec", spec);
        return new DynamicKubernetesObject(raw);
    }
}
