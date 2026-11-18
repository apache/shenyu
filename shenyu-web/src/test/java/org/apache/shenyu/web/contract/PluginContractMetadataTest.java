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

package org.apache.shenyu.web.contract;

import org.junit.jupiter.api.Test;

import java.util.EnumSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests the diagnostic metadata used by the published contract kit.
 */
public class PluginContractMetadataTest {

    @Test
    public void descriptionNamesKitCoreProfilesAndSkippedCapabilities() {
        final PluginContractMetadata metadata = new PluginContractMetadata("example",
                EnumSet.of(PluginContractProfile.TRANSFORM, PluginContractProfile.DOWNSTREAM_WRAPPER),
                Set.of("fork", "real-gateway-e2e"));
        final String description = metadata.describe();

        assertTrue(description.contains("plugin=example"));
        assertTrue(description.contains("kit=" + PluginContractMetadata.KIT_VERSION));
        assertTrue(description.contains("core=" + metadata.getCoreVersion()));
        assertTrue(description.contains("transform"));
        assertTrue(description.contains("downstream-wrapper"));
        assertTrue(description.contains("fork"));
        assertTrue(description.contains("real-gateway-e2e"));
    }

    @Test
    public void emptyProfilesAndSkippedCapabilitiesAreExplicit() {
        final PluginContractMetadata metadata = new PluginContractMetadata("example",
                EnumSet.noneOf(PluginContractProfile.class), Set.of());

        assertEquals("plugin=example, kit=0.1.0, core=" + metadata.getCoreVersion()
                + ", profiles=none, skipped=none", metadata.describe());
    }
}
