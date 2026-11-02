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

package org.apache.shenyu.admin.service.fencing;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RegistrationFencingTargetMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RegistrationFencingTargetDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationFencingAddress;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

/**
 * Common write guard shared by registration, diagnostics, provider and management writers.
 */
@Service
public class RegistrationFencingGuard {

    private final RegistrationFencingTargetMapper targetMapper;

    private final SelectorMapper selectorMapper;

    private final PluginMapper pluginMapper;

    public RegistrationFencingGuard(final RegistrationFencingTargetMapper targetMapper,
                                    final SelectorMapper selectorMapper, final PluginMapper pluginMapper) {
        this.targetMapper = targetMapper;
        this.selectorMapper = selectorMapper;
        this.pluginMapper = pluginMapper;
    }

    /**
     * Reject a legacy URI writer once the address has a fencing target.
     *
     * @param selectorId selector id
     * @param host host
     * @param port port
     */
    public void assertLegacyUriWrite(final String selectorId, final String host, final Integer port) {
        selectorMapper.selectByIdForUpdate(selectorId);
        RegistrationFencingTargetDO target;
        try {
            target = targetMapper.selectBySelectorAddressForUpdate(selectorId, RegistrationFencingAddress.canonicalHost(host), port);
        } catch (IllegalArgumentException exception) {
            return;
        }
        if (Objects.nonNull(target) && isProtecting(target)
                && !RegistrationFencingContext.matches(target.getId(), selectorId)) {
            throw conflict(RegistrationFencingProtocol.FENCING_REQUIRED, "fenced target requires a session");
        }
    }

    /**
     * Reject a legacy instance write when any protected target owns this address.
     * This is used before the asynchronous registration publisher accepts a request.
     *
     * @param namespaceId namespace id
     * @param host host
     * @param port port
     */
    public void assertLegacyUriWriteByAddress(final String namespaceId, final String host, final Integer port) {
        if (StringUtils.isBlank(namespaceId) || StringUtils.isBlank(host) || Objects.isNull(port)) {
            return;
        }
        if (!isProtectedAddress(namespaceId, host, port)) {
            return;
        }
        throw conflict(RegistrationFencingProtocol.FENCING_REQUIRED, "address requires a boot session");
    }

    /**
     * Reject configuration writers whose selector cannot be safely associated with
     * one fenced target.
     *
     * @param namespaceId namespace id
     */
    public void assertNamespaceConfigWriteAllowed(final String namespaceId) {
        if (StringUtils.isBlank(namespaceId) || Objects.nonNull(RegistrationFencingContext.current())) {
            return;
        }
        List<RegistrationFencingTargetDO> targets = targetMapper.selectByNamespaceId(namespaceId);
        if (hasProtectingTarget(targets)) {
            throw conflict(RegistrationFencingProtocol.FENCING_REQUIRED, "namespace has protected registration targets");
        }
    }

    /**
     * Reject selector-wide writers such as provider callbacks and management imports.
     *
     * @param selectorId selector id
     */
    public void assertSelectorWriteAllowed(final String selectorId) {
        if (StringUtils.isBlank(selectorId) || RegistrationFencingContext.matchesSelector(selectorId)) {
            return;
        }
        selectorMapper.selectByIdForUpdate(selectorId);
        // The caller may already have a repeatable-read snapshot from before target creation.
        // Locking the selector serializes writers but does not refresh ordinary target reads.
        List<RegistrationFencingTargetDO> targets = targetMapper.selectBySelectorIdForUpdate(selectorId);
        if (hasProtectingTarget(targets)) {
            throw conflict(RegistrationFencingProtocol.FENCING_REQUIRED, "selector is protected by a boot session");
        }
    }

    /**
     * Reject a legacy write only when the concrete selector address is protected.
     * The selector row lock closes the enablement race with target creation.
     *
     * @param selectorId selector id
     * @param url upstream URL
     */
    public void assertSelectorAddressWriteAllowed(final String selectorId, final String url) {
        if (StringUtils.isBlank(selectorId) || StringUtils.isBlank(url) || RegistrationFencingContext.matchesSelector(selectorId)) {
            return;
        }
        selectorMapper.selectByIdForUpdate(selectorId);
        if (isSelectorAddressProtectedAfterLock(selectorId, url)) {
            throw conflict(RegistrationFencingProtocol.FENCING_REQUIRED, "fenced target requires a session");
        }
    }

    /**
     * Check whether an address is protected while holding the selector row lock.
     *
     * @param selectorId selector id
     * @param url upstream URL
     * @return whether the address is protected
     */
    public boolean isSelectorAddressProtected(final String selectorId, final String url) {
        if (StringUtils.isBlank(selectorId) || StringUtils.isBlank(url) || RegistrationFencingContext.matchesSelector(selectorId)) {
            return false;
        }
        selectorMapper.selectByIdForUpdate(selectorId);
        return isSelectorAddressProtectedAfterLock(selectorId, url);
    }

    private boolean isSelectorAddressProtectedAfterLock(final String selectorId, final String url) {
        final String endpoint;
        try {
            endpoint = RegistrationFencingAddress.canonicalEndpoint(url);
        } catch (IllegalArgumentException exception) {
            return false;
        }
        int separator = endpoint.lastIndexOf(':');
        String host = endpoint.substring(0, separator);
        Integer port = Integer.valueOf(endpoint.substring(separator + 1));
        RegistrationFencingTargetDO target = targetMapper.selectBySelectorAddressForUpdate(selectorId, host, port);
        return Objects.nonNull(target) && isProtecting(target);
    }

    /**
     * Reject a metadata/config writer when its existing selector is protected.
     *
     * @param selectorName selector name
     * @param pluginName plugin name
     * @param namespaceId namespace id
     */
    public void assertMetadataWriteAllowed(final String selectorName, final String pluginName, final String namespaceId) {
        if (StringUtils.isBlank(selectorName) || StringUtils.isBlank(pluginName) || StringUtils.isBlank(namespaceId)) {
            return;
        }
        PluginDO plugin = pluginMapper.selectByName(pluginName);
        if (Objects.isNull(plugin)) {
            return;
        }
        SelectorDO selector = selectorMapper.findByNameAndPluginIdAndNamespaceId(selectorName, plugin.getId(), namespaceId);
        if (Objects.nonNull(selector)) {
            assertSelectorWriteAllowed(selector.getId());
        }
    }

    /**
     * Check whether an instance observation belongs to any protected target.
     * The event format has no selector id, so the conservative address policy
     * intentionally blocks ambiguous observations as required by the contract.
     *
     * @param namespaceId namespace id
     * @param host host
     * @param port port
     * @return whether the address is protected
     */
    public boolean isProtectedAddress(final String namespaceId, final String host, final Integer port) {
        final String canonicalHost;
        try {
            canonicalHost = RegistrationFencingAddress.canonicalHost(host);
        } catch (IllegalArgumentException exception) {
            return false;
        }
        List<RegistrationFencingTargetDO> targets = targetMapper.selectByNamespaceAddress(namespaceId, canonicalHost, port);
        return hasProtectingTarget(targets);
    }

    /**
     * Check whether a selector has a protected target.
     *
     * @param selectorId selector id
     * @return protected flag
     */
    public boolean isProtectedSelector(final String selectorId) {
        List<RegistrationFencingTargetDO> targets = targetMapper.selectBySelectorId(selectorId);
        return hasProtectingTarget(targets);
    }

    private boolean hasProtectingTarget(final List<RegistrationFencingTargetDO> targets) {
        return Objects.nonNull(targets) && targets.stream().anyMatch(this::isProtecting);
    }

    /**
     * A disabled target keeps its tombstone for history but no longer fences writes.
     * Only an enabled target protects its selector and members.
     *
     * @param target target row
     * @return whether the target still protects its members
     */
    private boolean isProtecting(final RegistrationFencingTargetDO target) {
        return Boolean.TRUE.equals(target.getEnabled());
    }

    private RegistrationFencingException conflict(final String code, final String message) {
        return new RegistrationFencingException(code, message);
    }
}
