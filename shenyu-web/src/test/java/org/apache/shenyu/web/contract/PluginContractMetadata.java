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

import org.apache.shenyu.plugin.api.ShenyuPlugin;

import java.util.Comparator;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Identifies the contract kit, the core artifact and the profiles used by a test run.
 *
 * <p>The description is intentionally plain text so a failing build can include it without
 * another reporting dependency. The core version comes from the packaged plugin API manifest; a
 * development classpath without a manifest reports {@code development}.</p>
 */
public final class PluginContractMetadata {

    /**
     * The test-kit version, independent from the gateway version.
     */
    public static final String KIT_VERSION = "0.1.0";

    private final String pluginName;

    private final String coreVersion;

    private final Set<PluginContractProfile> profiles;

    private final Set<String> skippedCapabilities;

    /**
     * Creates metadata for a test run.
     *
     * @param pluginName plugin under test
     * @param profiles selected profiles
     * @param skippedCapabilities capabilities intentionally not exercised
     */
    public PluginContractMetadata(final String pluginName, final Set<PluginContractProfile> profiles,
                                  final Set<String> skippedCapabilities) {
        this.pluginName = Objects.requireNonNull(pluginName, "pluginName");
        this.coreVersion = currentCoreVersion();
        this.profiles = Set.copyOf(Objects.requireNonNull(profiles, "profiles"));
        this.skippedCapabilities = Set.copyOf(Objects.requireNonNull(skippedCapabilities, "skippedCapabilities"));
    }

    /**
     * Returns the core plugin API version available on the test classpath.
     *
     * @return version or {@code development}
     */
    public static String currentCoreVersion() {
        final Package pluginPackage = ShenyuPlugin.class.getPackage();
        final String version = Objects.isNull(pluginPackage) ? null : pluginPackage.getImplementationVersion();
        return Objects.isNull(version) ? "development" : version;
    }

    /**
     * Returns the plugin name.
     *
     * @return plugin name
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * Returns the core version.
     *
     * @return core version
     */
    public String getCoreVersion() {
        return coreVersion;
    }

    /**
     * Returns the selected profiles.
     *
     * @return immutable profiles
     */
    public Set<PluginContractProfile> getProfiles() {
        return profiles;
    }

    /**
     * Returns the intentionally skipped capabilities.
     *
     * @return immutable skipped capabilities
     */
    public Set<String> getSkippedCapabilities() {
        return skippedCapabilities;
    }

    /**
     * Returns a stable one-line description for build logs.
     *
     * @return description
     */
    public String describe() {
        final String profileNames = profiles.stream()
                .sorted(Comparator.comparing(PluginContractProfile::name))
                .map(PluginContractProfile::displayName)
                .collect(Collectors.joining(","));
        final String skippedNames = skippedCapabilities.stream()
                .sorted()
                .collect(Collectors.joining(","));
        return "plugin=" + pluginName
                + ", kit=" + KIT_VERSION
                + ", core=" + coreVersion
                + ", profiles=" + (profileNames.isEmpty() ? "none" : profileNames)
                + ", skipped=" + (skippedNames.isEmpty() ? "none" : skippedNames);
    }
}
