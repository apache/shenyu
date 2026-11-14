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

import io.grpc.Attributes;
import io.grpc.CallOptions;
import io.grpc.ConnectivityState;
import io.grpc.ConnectivityStateInfo;
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;
import org.apache.shenyu.plugin.grpc.context.GrpcConstants;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannels;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ShenyuPicker}.
 */
public class ShenyuPickerTest {

    @Test
    public void testDelayedPickUsesCallOptionsWithoutRequestContext() {
        Attributes attributes = SubChannels.createAttributes(1, "true", "127.0.0.1:3888");
        LoadBalancer.Subchannel subchannel = SubChannels.createSubChannel(new UnitTestReadHelper(), mock(EquivalentAddressGroup.class), attributes);
        SubChannels.setStateInfo(subchannel, ConnectivityStateInfo.forNonError(ConnectivityState.READY));
        ShenyuPicker picker = new ShenyuPicker(List.of(subchannel));
        CallOptions callOptions = CallOptions.DEFAULT
                .withOption(GrpcConstants.GRPC_SELECTOR_ID_OPTION, "selectorId")
                .withOption(GrpcConstants.GRPC_RULE_ID_OPTION, "ruleId")
                .withOption(GrpcConstants.GRPC_REMOTE_ADDRESS_OPTION, "127.0.0.1");
        LoadBalancer.PickSubchannelArgs args = mock(LoadBalancer.PickSubchannelArgs.class);
        when(args.getCallOptions()).thenReturn(callOptions);

        LoadBalancer.PickResult result = CompletableFuture.supplyAsync(() -> picker.pickSubchannel(args)).join();

        assertNotNull(result.getSubchannel());
    }
}
