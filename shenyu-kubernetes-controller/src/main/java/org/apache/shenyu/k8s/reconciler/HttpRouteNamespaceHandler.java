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

import io.kubernetes.client.extended.controller.reconciler.Request;
import io.kubernetes.client.extended.workqueue.RateLimitingQueue;
import io.kubernetes.client.informer.ResourceEventHandler;
import io.kubernetes.client.informer.SharedIndexInformer;
import io.kubernetes.client.informer.cache.Lister;
import io.kubernetes.client.openapi.models.V1Namespace;
import io.kubernetes.client.openapi.models.V1ObjectMeta;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * Reconciles every route in a Namespace when its selector labels change, including
 * rejected routes that have no existing binding and may now be allowed to attach.
 */
public final class HttpRouteNamespaceHandler implements ResourceEventHandler<V1Namespace> {

    private final Lister<DynamicKubernetesObject> httpRouteLister;

    private final RateLimitingQueue<Request> httpRouteWorkQueue;

    public HttpRouteNamespaceHandler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                                     final RateLimitingQueue<Request> httpRouteWorkQueue) {
        this.httpRouteLister = new Lister<>(httpRouteInformer.getIndexer());
        this.httpRouteWorkQueue = httpRouteWorkQueue;
    }

    @Override
    public void onAdd(final V1Namespace namespace) {
        enqueueRoutes(namespace);
    }

    @Override
    public void onUpdate(final V1Namespace oldNamespace, final V1Namespace newNamespace) {
        if (!Objects.equals(labelsOf(oldNamespace), labelsOf(newNamespace))) {
            enqueueRoutes(newNamespace);
        }
    }

    @Override
    public void onDelete(final V1Namespace namespace, final boolean unknownState) {
        enqueueRoutes(namespace);
    }

    private Map<String, String> labelsOf(final V1Namespace namespace) {
        V1ObjectMeta metadata = Objects.isNull(namespace) ? null : namespace.getMetadata();
        return Objects.isNull(metadata) || Objects.isNull(metadata.getLabels())
                ? Collections.emptyMap() : metadata.getLabels();
    }

    private void enqueueRoutes(final V1Namespace namespace) {
        V1ObjectMeta metadata = Objects.isNull(namespace) ? null : namespace.getMetadata();
        if (Objects.isNull(metadata) || Objects.isNull(metadata.getName()) || metadata.getName().isEmpty()) {
            return;
        }
        // Default informer caches provide the namespace index used by Lister.namespace().list().
        for (DynamicKubernetesObject route : httpRouteLister.namespace(metadata.getName()).list()) {
            V1ObjectMeta routeMetadata = route.getMetadata();
            if (Objects.nonNull(routeMetadata) && Objects.nonNull(routeMetadata.getNamespace()) && Objects.nonNull(routeMetadata.getName())) {
                httpRouteWorkQueue.add(new Request(routeMetadata.getNamespace(), routeMetadata.getName()));
            }
        }
    }
}
