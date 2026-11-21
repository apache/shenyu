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
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.context.GrpcConstants;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannels;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ShenyuPicker}.
 */
public class ShenyuPickerTest {

    @Test
    public void testDelayedPickUsesCallOptionsWithoutRequestContext() {
        String selectorId = "delayed-selector";
        String ruleId = "delayed-rule";
        String firstUrl = "127.0.0.1:3888";
        String secondUrl = "127.0.0.1:3889";
        LoadBalancer.Subchannel first = readySubchannel(firstUrl);
        LoadBalancer.Subchannel second = readySubchannel(secondUrl);
        ShenyuPicker picker = new ShenyuPicker(List.of(first, second));
        CallOptions callOptions = CallOptions.DEFAULT
                .withOption(GrpcConstants.GRPC_SELECTOR_ID_OPTION, selectorId)
                .withOption(GrpcConstants.GRPC_RULE_ID_OPTION, ruleId)
                .withOption(GrpcConstants.GRPC_REMOTE_ADDRESS_OPTION, "192.0.2.10");
        LoadBalancer.PickSubchannelArgs args = mock(LoadBalancer.PickSubchannelArgs.class);
        when(args.getCallOptions()).thenReturn(callOptions);

        ApplicationConfigCache cache = ApplicationConfigCache.getInstance();
        cache.cacheRuleHandle(ruleId, "{\"loadBalance\":\"hash\"}");
        cache.handlerUpstream(selectorId, List.of(upstream(firstUrl), upstream(secondUrl)));
        try {
            LoadBalancer.PickResult result = CompletableFuture.supplyAsync(() -> {
                try (MockedStatic<LoadBalancerFactory> factory = mockStatic(LoadBalancerFactory.class)) {
                    factory.when(() -> LoadBalancerFactory.selector(anyList(), eq("hash"), any(LoadBalanceData.class)))
                            .thenAnswer(invocation -> {
                                List<Upstream> upstreams = invocation.getArgument(0);
                                LoadBalanceData data = invocation.getArgument(2);
                                assertEquals(List.of(firstUrl, secondUrl), upstreams.stream().map(Upstream::getUrl).toList());
                                assertEquals("192.0.2.10", data.getIp());
                                return upstreams.get(1);
                            });
                    LoadBalancer.PickResult pick = picker.pickSubchannel(args);
                    factory.verify(() -> LoadBalancerFactory.selector(anyList(), eq("hash"), any(LoadBalanceData.class)));
                    return pick;
                }
            }).join();
            assertSame(second, result.getSubchannel());
        } finally {
            cache.removeRuleHandle(ruleId);
            cache.handlerUpstream(selectorId, List.of());
        }
    }

    @Test
    public void testMissingRoutingMetadataFallsBackToReadySubchannel() {
        LoadBalancer.Subchannel subchannel = readySubchannel("127.0.0.1:3888");
        ShenyuPicker picker = new ShenyuPicker(List.of(subchannel));
        assertSame(subchannel, picker.pickSubchannel(null).getSubchannel());

        LoadBalancer.PickSubchannelArgs args = mock(LoadBalancer.PickSubchannelArgs.class);
        when(args.getCallOptions()).thenReturn(CallOptions.DEFAULT);
        assertSame(subchannel, picker.pickSubchannel(args).getSubchannel());

        when(args.getCallOptions()).thenReturn(CallOptions.DEFAULT
                .withOption(GrpcConstants.GRPC_SELECTOR_ID_OPTION, "selector")
                .withOption(GrpcConstants.GRPC_RULE_ID_OPTION, "missing-rule"));
        assertSame(subchannel, picker.pickSubchannel(args).getSubchannel());
    }

    private LoadBalancer.Subchannel readySubchannel(final String url) {
        Attributes attributes = SubChannels.createAttributes(1, "true", url);
        LoadBalancer.Subchannel subchannel = SubChannels.createSubChannel(new UnitTestReadHelper(), mock(EquivalentAddressGroup.class), attributes);
        SubChannels.setStateInfo(subchannel, ConnectivityStateInfo.forNonError(ConnectivityState.READY));
        return subchannel;
    }

    private GrpcUpstream upstream(final String url) {
        return GrpcUpstream.builder().upstreamUrl(url).status(true).build();
    }
}
