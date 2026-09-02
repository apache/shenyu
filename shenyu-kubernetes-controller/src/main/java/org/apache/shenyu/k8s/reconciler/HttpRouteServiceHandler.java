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
import io.kubernetes.client.openapi.models.V1Service;
import io.kubernetes.client.util.generic.dynamic.DynamicKubernetesObject;

/**
 * Bridges Service events to HTTPRoute reconciliation. A Service port or targetPort change
 * does not touch the Endpoints object, so without this handler routes would keep routing
 * to the stale pod port until the periodic HTTPRoute resync; an initial reconcile could
 * likewise fall back to the Endpoints-only port heuristic before the Service cache filled.
 * See {@link HttpRouteBackendHandler} for the index shared with the Endpoints handler.
 */
public final class HttpRouteServiceHandler extends HttpRouteBackendHandler implements ResourceEventHandler<V1Service> {

    public HttpRouteServiceHandler(final SharedIndexInformer<DynamicKubernetesObject> httpRouteInformer,
                                   final RateLimitingQueue<Request> httpRouteWorkQueue) {
        super(httpRouteInformer, httpRouteWorkQueue);
    }

    @Override
    public void onAdd(final V1Service service) {
        enqueueAffectedRoutes(namespaceOf(service.getMetadata()), nameOf(service.getMetadata()));
    }

    @Override
    public void onUpdate(final V1Service oldService, final V1Service newService) {
        enqueueAffectedRoutes(namespaceOf(newService.getMetadata()), nameOf(newService.getMetadata()));
    }

    @Override
    public void onDelete(final V1Service service, final boolean unknownState) {
        enqueueAffectedRoutes(namespaceOf(service.getMetadata()), nameOf(service.getMetadata()));
    }
}
