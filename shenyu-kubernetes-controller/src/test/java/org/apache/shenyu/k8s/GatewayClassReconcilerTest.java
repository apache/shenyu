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
import org.apache.shenyu.k8s.reconciler.GatewayClassReconciler;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * GatewayClass Reconciler Test: acceptance, foreign classes, and the ownership-loss
 * transition (controllerName re-pointed away from ShenYu) which must re-queue previously
 * served Gateways immediately instead of waiting for their resync.
 */
public final class GatewayClassReconcilerTest {

    private static final String SHENYU_CONTROLLER = "gateway.shenyu.apache.org/shenyu-controller";

    private RateLimitingQueue<Request> gatewayWorkQueue;

    private ApiClient reconcilerApi;

    @BeforeEach
    public void setUp() {
        GatewayRouteCache.getInstance().clear();
    }

    /**
     * A ShenYu GatewayClass without Accepted status gets Accepted=True patched.
     */
    @Test
    public void testShenYuGatewayClassGetsAcceptedStatus() throws Exception {
        GatewayClassReconciler reconciler = reconciler(
                gatewayClass("shenyu", SHENYU_CONTROLLER, null), null);

        Result result = reconciler.reconcile(new Request("", "shenyu"));
        Assertions.assertEquals(new Result(false), result);
        verify(reconcilerApi).execute(any(okhttp3.Call.class));
    }

    /**
     * Ownership loss: the class was accepted by ShenYu (our Accepted=True payload) and its
     * controllerName moved to another controller. Gateways previously served through it
     * must be re-queued for immediate cleanup and the stale Accepted entry downgraded.
     */
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
    }

    /**
     * A foreign class ShenYu never served (no bindings, no ShenYu-written status) must be
     * skipped entirely: re-queuing its Gateways or patching its status would fight the
     * controller that owns it.
     */
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

    private GatewayClassReconciler reconciler(final DynamicKubernetesObject gatewayClass,
                                              final DynamicKubernetesObject gateway) {
        SharedIndexInformer<DynamicKubernetesObject> gatewayClassInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayClassIndexer = mock(Indexer.class);
        when(gatewayClassIndexer.getByKey("shenyu")).thenReturn(gatewayClass);
        when(gatewayClassIndexer.getByKey("other-class")).thenReturn(gatewayClass);
        when(gatewayClassInformer.getIndexer()).thenReturn(gatewayClassIndexer);

        SharedIndexInformer<DynamicKubernetesObject> gatewayInformer = mock(SharedIndexInformer.class);
        Indexer<DynamicKubernetesObject> gatewayIndexer = mock(Indexer.class);
        if (Objects.nonNull(gateway)) {
            when(gatewayIndexer.list()).thenReturn(List.of(gateway));
        }
        when(gatewayInformer.getIndexer()).thenReturn(gatewayIndexer);

        gatewayWorkQueue = mock(RateLimitingQueue.class);
        reconcilerApi = mock(ApiClient.class);
        try {
            when(reconcilerApi.getAuthentications()).thenReturn(Map.of());
            when(reconcilerApi.buildCall(any(), any(), any(), any(), any(), any(), any(), any(), any(), any()))
                    .thenReturn(mock(okhttp3.Call.class));
        } catch (ApiException e) {
            throw new IllegalStateException(e);
        }
        return new GatewayClassReconciler(gatewayClassInformer, gatewayInformer, gatewayWorkQueue, reconcilerApi);
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
