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
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannelCopy;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannels;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import static org.mockito.Mockito.mock;

@RunWith(MockitoJUnitRunner.class)
public class RoundRobinPickerTest {

    private RoundRobinPicker roundRobinPicker;

    private List<LoadBalancer.Subchannel> list;

    @Before
    public void setUp() {
        Attributes attributes = SubChannels.createAttributes(1, "ok");
        LoadBalancer.Subchannel subchannel =
                SubChannels.createSubChannel(new UnitTestReadHelper(), mock(EquivalentAddressGroup.class), attributes);
        list = new LinkedList<>();
        list.add(subchannel);
        roundRobinPicker = new RoundRobinPicker(list);
    }

    @Test
    public void testPick() {
        SubChannelCopy firstSubChannelCopy = mock(SubChannelCopy.class);
        SubChannelCopy secondSubChannelCopy = mock(SubChannelCopy.class);
        List<SubChannelCopy> list = Arrays.asList(firstSubChannelCopy, secondSubChannelCopy);
        Assert.assertNotNull(roundRobinPicker.pick(list));
        Assert.assertNotNull(roundRobinPicker.pick(list));
        Assert.assertEquals(firstSubChannelCopy, roundRobinPicker.pick(Arrays.asList(firstSubChannelCopy)));
        Assert.assertNull(roundRobinPicker.pick(null));
    }
}
