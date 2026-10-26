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

package org.apache.shenyu.springboot.starter.sync.data.websocket;

import org.springframework.boot.actuate.endpoint.SecurityContext;
import org.springframework.boot.actuate.health.AdditionalHealthEndpointPath;
import org.springframework.boot.actuate.health.HealthEndpointGroup;
import org.springframework.boot.actuate.health.HttpCodeStatusMapper;
import org.springframework.boot.actuate.health.StatusAggregator;

/**
 * Adds synchronization to readiness while excluding it from aggregate and liveness health.
 */
final class WebsocketSyncHealthGroup implements HealthEndpointGroup {

    private final HealthEndpointGroup delegate;

    private final boolean readiness;

    WebsocketSyncHealthGroup(final HealthEndpointGroup delegate, final boolean readiness) {
        this.delegate = delegate;
        this.readiness = readiness;
    }

    @Override
    public boolean isMember(final String name) {
        return "websocketSync".equals(name) ? readiness : delegate.isMember(name);
    }

    @Override
    public boolean showComponents(final SecurityContext securityContext) {
        return delegate.showComponents(securityContext);
    }

    @Override
    public boolean showDetails(final SecurityContext securityContext) {
        return delegate.showDetails(securityContext);
    }

    @Override
    public StatusAggregator getStatusAggregator() {
        return delegate.getStatusAggregator();
    }

    @Override
    public HttpCodeStatusMapper getHttpCodeStatusMapper() {
        return delegate.getHttpCodeStatusMapper();
    }

    @Override
    public AdditionalHealthEndpointPath getAdditionalPath() {
        return delegate.getAdditionalPath();
    }
}
