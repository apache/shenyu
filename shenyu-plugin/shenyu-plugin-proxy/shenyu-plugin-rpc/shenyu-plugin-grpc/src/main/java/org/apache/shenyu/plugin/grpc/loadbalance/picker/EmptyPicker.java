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

package org.apache.shenyu.plugin.grpc.loadbalance.picker;

import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import io.grpc.LoadBalancer;
import io.grpc.Status;

/**
 * The empty picker.
 */
public class EmptyPicker extends AbstractPicker {

    private final Status status;

    public EmptyPicker(final Status status) {
        this.status = Preconditions.checkNotNull(status, "status");
    }

    @Override
    public LoadBalancer.PickResult pickSubchannel(final LoadBalancer.PickSubchannelArgs args) {
        return status.isOk() ? LoadBalancer.PickResult.withNoResult() : LoadBalancer.PickResult.withError(status);
    }

    @Override
    public boolean isEquivalentTo(final AbstractPicker picker) {
        return picker instanceof EmptyPicker && (Objects.equal(status, ((EmptyPicker) picker).status) || status.isOk() && ((EmptyPicker) picker).status.isOk());
    }

    @Override
    public String getSubchannelsInfo() {
        return "[]";
    }
}
