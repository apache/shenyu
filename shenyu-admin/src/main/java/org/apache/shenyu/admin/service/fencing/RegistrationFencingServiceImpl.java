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
import org.apache.shenyu.admin.config.properties.RegistrationFencingProperties;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RegistrationFencingTargetMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RegistrationFencingTargetDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.register.FencedRegisterService;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.model.enums.DiscoveryTypeEnum;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationFencingAddress;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Shared-database state machine for registration boot-session fencing.
 */
@Service
public class RegistrationFencingServiceImpl implements RegistrationFencingService {

    private static final String NEVER = "NEVER";

    private static final String ACTIVE = "ACTIVE";

    private static final String RELEASED = "RELEASED";

    private static final String DISABLED = "DISABLED";

    private final RegistrationFencingTargetMapper targetMapper;

    private final SelectorMapper selectorMapper;

    private final PluginMapper pluginMapper;

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final DiscoveryHandlerMapper discoveryHandlerMapper;

    private final DiscoveryMapper discoveryMapper;

    private final RegistrationFencingProperties properties;

    private final ConcurrentHashMap<String, FencedRegisterService> registerServices = new ConcurrentHashMap<>();

    /**
     * Create the fencing service.
     *
     * @param targetMapper target mapper
     * @param selectorMapper selector mapper
     * @param pluginMapper plugin mapper
     * @param discoveryUpstreamMapper discovery upstream mapper
     * @param properties fencing properties
     * @param services fenced register services
     */
    public RegistrationFencingServiceImpl(final RegistrationFencingTargetMapper targetMapper,
                                          final SelectorMapper selectorMapper,
                                          final PluginMapper pluginMapper,
                                          final DiscoveryUpstreamMapper discoveryUpstreamMapper,
                                          final DiscoveryHandlerMapper discoveryHandlerMapper,
                                          final DiscoveryMapper discoveryMapper,
                                          final RegistrationFencingProperties properties,
                                          final List<FencedRegisterService> services) {
        this.targetMapper = targetMapper;
        this.selectorMapper = selectorMapper;
        this.pluginMapper = pluginMapper;
        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
        this.discoveryHandlerMapper = discoveryHandlerMapper;
        this.discoveryMapper = discoveryMapper;
        this.properties = properties;
        services.forEach(service -> registerServices.put(service.rpcType(), service));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData> createTarget(
            final RegistrationFencingProtocol.TargetRequest request, final boolean root) {
        requireEnabled();
        if (!root) {
            return error(RegistrationFencingProtocol.FORBIDDEN, "root principal required");
        }
        validateTargetRequest(request);
        SelectorDO selector = selectorMapper.selectByIdForUpdate(request.selectorId());
        if (Objects.isNull(selector)
                || !Objects.equals(selector.getNamespaceId(), request.namespaceId())) {
            return error(RegistrationFencingProtocol.SOURCE_CONFLICT, "selector binding does not match");
        }
        PluginDO plugin = pluginMapper.selectById(selector.getPluginId());
        if (Objects.isNull(plugin) || StringUtils.isBlank(plugin.getName())) {
            return error(RegistrationFencingProtocol.SOURCE_CONFLICT, "selector plugin does not exist");
        }
        String canonicalHost;
        String canonicalProtocol;
        try {
            canonicalHost = RegistrationFencingAddress.canonicalHost(request.host());
            canonicalProtocol = RegistrationFencingAddress.canonicalProtocol(request.protocol());
        } catch (IllegalArgumentException exception) {
            return error(RegistrationFencingProtocol.INVALID_REQUEST, exception.getMessage());
        }
        String expectedUrl = CommonUpstreamUtils.buildUrl(canonicalHost, request.port());
        List<DiscoveryUpstreamDO> upstreams = discoveryUpstreamMapper.selectBySelectorId(request.selectorId());
        boolean bound = Objects.nonNull(upstreams) && upstreams.stream().anyMatch(upstream ->
                Objects.equals(expectedUrl, canonicalEndpoint(upstream.getUpstreamUrl()))
                        && Objects.equals(request.namespaceId(), upstream.getNamespaceId())
                        && Objects.equals(canonicalProtocol, canonicalProtocol(upstream.getProtocol())));
        if (!bound) {
            return error(RegistrationFencingProtocol.SOURCE_CONFLICT, "target address is not an existing selector upstream");
        }
        DiscoveryHandlerDO discoveryHandler = discoveryHandlerMapper.selectBySelectorId(request.selectorId());
        if (Objects.nonNull(discoveryHandler)) {
            DiscoveryDO discovery = discoveryMapper.selectById(discoveryHandler.getDiscoveryId());
            if (Objects.nonNull(discovery) && !DiscoveryTypeEnum.LOCAL.getType().equals(discovery.getDiscoveryType())) {
                return error(RegistrationFencingProtocol.SOURCE_CONFLICT, "provider-managed selectors cannot be fenced");
            }
        }
        RegistrationFencingTargetDO existing = targetMapper.selectBySelectorAddress(request.selectorId(), canonicalHost, request.port());
        if (Objects.nonNull(existing)) {
            return error(RegistrationFencingProtocol.SOURCE_CONFLICT, "target address is already protected");
        }
        Timestamp now = new Timestamp(System.currentTimeMillis());
        RegistrationFencingTargetDO target = new RegistrationFencingTargetDO();
        target.setId(UUIDUtils.getInstance().generateShortUuid());
        target.setDateCreated(now);
        target.setDateUpdated(now);
        target.setDomainId(request.domainId());
        target.setNamespaceId(request.namespaceId());
        target.setSelectorId(request.selectorId());
        target.setPluginId(plugin.getId());
        target.setProtocol(canonicalProtocol);
        target.setHost(canonicalHost);
        target.setPort(request.port());
        target.setPrincipalId(request.principalId());
        target.setEnabled(Boolean.TRUE);
        target.setState(NEVER);
        target.setGeneration(0L);
        target.setLastSequence(0L);
        targetMapper.insert(target);
        return RegistrationFencingProtocol.Response.ok(toTargetData(target));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> replace(
            final String targetId, final RegistrationFencingProtocol.ReplaceRequest request, final boolean root) {
        requireEnabled();
        if (!root) {
            return error(RegistrationFencingProtocol.FORBIDDEN, "root principal required");
        }
        if (StringUtils.isBlank(targetId) || Objects.isNull(request)
                || !isUuid(request.requestId()) || !isUuid(request.startupId())) {
            return error(RegistrationFencingProtocol.INVALID_REQUEST, "invalid replacement request");
        }
        final long expectedGeneration = decimal(request.expectedGeneration());
        RegistrationFencingTargetDO target = lockTargetWithSelector(targetId);
        if (Objects.isNull(target)) {
            return error(RegistrationFencingProtocol.TARGET_NOT_FOUND, "target does not exist");
        }
        String requestPayload = GsonUtils.getInstance().toJson(request);
        if (Objects.equals(request.requestId(), target.getLastRequestId())
                && Objects.equals(requestPayload, target.getLastRequestPayload())) {
            return RegistrationFencingProtocol.Response.ok(toSessionData(target));
        }
        if (Objects.equals(request.requestId(), target.getLastRequestId())) {
            return error(RegistrationFencingProtocol.IDEMPOTENCY_CONFLICT, "request id was reused with different content");
        }
        if (!Objects.equals(target.getGeneration(), expectedGeneration)
                || !Objects.equals(nullToEmpty(target.getOwnerStartupId()), nullToEmpty(request.expectedStartupId()))) {
            return error(RegistrationFencingProtocol.CAS_MISMATCH, "owner or generation changed");
        }
        if (!target.getEnabled()) {
            return error(RegistrationFencingProtocol.FENCING_REQUIRED, "target is disabled");
        }
        grant(target, request.startupId(), request.principalId());
        target.setLastRequestId(request.requestId());
        target.setLastRequestPayload(requestPayload);
        targetMapper.update(target);
        return RegistrationFencingProtocol.Response.ok(toSessionData(target));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public RegistrationFencingProtocol.Response<RegistrationFencingProtocol.TargetData> disable(final String targetId,
                                                                                                 final boolean root) {
        requireEnabled();
        if (!root) {
            return error(RegistrationFencingProtocol.FORBIDDEN, "root principal required");
        }
        RegistrationFencingTargetDO target = lockTargetWithSelector(targetId);
        if (Objects.isNull(target)) {
            return error(RegistrationFencingProtocol.TARGET_NOT_FOUND, "target does not exist");
        }
        target.setEnabled(Boolean.FALSE);
        target.setState(DISABLED);
        target.setGeneration(target.getGeneration() + 1L);
        target.setOwnerStartupId(null);
        target.setSessionId(null);
        target.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        targetMapper.update(target);
        return RegistrationFencingProtocol.Response.ok(toTargetData(target));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public RegistrationFencingProtocol.Response<RegistrationFencingProtocol.SessionData> acquire(
            final RegistrationFencingProtocol.AcquireRequest request, final String principalId) {
        requireEnabled();
        if (Objects.isNull(request) || !isUuid(request.startupId()) || StringUtils.isBlank(principalId)) {
            return error(RegistrationFencingProtocol.INVALID_REQUEST, "invalid acquire request");
        }
        final long expectedGeneration = decimal(request.expectedGeneration());
        RegistrationFencingTargetDO target = lockTargetWithSelector(request.targetId());
        if (Objects.isNull(target)) {
            return error(RegistrationFencingProtocol.TARGET_NOT_FOUND, "target does not exist");
        }
        if (!Objects.equals(target.getDomainId(), request.domainId())
                || !Objects.equals(target.getPrincipalId(), principalId)) {
            return error(RegistrationFencingProtocol.FORBIDDEN, "target principal or domain does not match");
        }
        if (!target.getEnabled()) {
            return error(RegistrationFencingProtocol.FENCING_REQUIRED, "target is disabled");
        }
        if (ACTIVE.equals(target.getState()) && Objects.equals(target.getOwnerStartupId(), request.startupId())) {
            return RegistrationFencingProtocol.Response.ok(toSessionData(target));
        }
        if (ACTIVE.equals(target.getState())) {
            return error(RegistrationFencingProtocol.OWNER_CONFLICT, "target has another owner");
        }
        if (!Objects.equals(target.getGeneration(), expectedGeneration)) {
            return error(RegistrationFencingProtocol.CAS_MISMATCH, "generation or startup is not eligible");
        }
        grant(target, request.startupId(), principalId);
        targetMapper.update(target);
        return RegistrationFencingProtocol.Response.ok(toSessionData(target));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public RegistrationFencingProtocol.Response<RegistrationFencingProtocol.MutationData> mutate(
            final RegistrationFencingProtocol.MutationRequest request, final String principalId) {
        requireEnabled();
        if (Objects.isNull(request) || StringUtils.isBlank(principalId)
                || !isUuid(request.sessionId()) || !validOperation(request.operation())
                || Objects.isNull(request.payload())) {
            return error(RegistrationFencingProtocol.INVALID_REQUEST, "invalid mutation request");
        }
        final long generation = decimal(request.generation());
        final long sequence = decimal(request.clientSequence());
        if (sequence < 1L) {
            return error(RegistrationFencingProtocol.INVALID_REQUEST, "clientSequence starts at 1");
        }
        RegistrationFencingTargetDO sessionTarget = targetMapper.selectBySessionId(request.domainId(), request.sessionId());
        if (Objects.isNull(sessionTarget)) {
            return error(RegistrationFencingProtocol.STALE_SESSION, "session is not active");
        }
        RegistrationFencingTargetDO target = lockTargetWithSelector(sessionTarget.getId());
        if (Objects.isNull(target) || !Objects.equals(target.getDomainId(), request.domainId())
                || !Objects.equals(target.getSelectorId(), request.selectorId())
                || !Objects.equals(target.getSessionId(), request.sessionId())
                || !Objects.equals(target.getPrincipalId(), principalId)) {
            return error(RegistrationFencingProtocol.STALE_SESSION, "session is not active");
        }
        URIRegisterDTO payload = canonicalPayload(request.payload());
        SelectorDO selector = selectorMapper.selectById(target.getSelectorId());
        if (Objects.isNull(selector) || !Objects.equals(selector.getNamespaceId(), payload.getNamespaceId())
                || !Objects.equals(selector.getSelectorName(), payloadServiceName(payload))) {
            return error(RegistrationFencingProtocol.SOURCE_CONFLICT, "payload service is not bound to selector");
        }
        final RegistrationFencingProtocol.MutationRequest effectiveRequest = new RegistrationFencingProtocol.MutationRequest(
                request.domainId(), request.sessionId(), request.selectorId(), request.generation(), request.clientSequence(), request.operation(), payload);
        if (!Objects.equals(target.getHost(), payload.getHost())
                || !Objects.equals(target.getPort(), request.payload().getPort())
                || !Objects.equals(target.getNamespaceId(), payload.getNamespaceId())
                || !Objects.equals(target.getProtocol(), payload.getProtocol())) {
            return error(RegistrationFencingProtocol.SOURCE_CONFLICT, "payload address is not bound to target");
        }
        String canonical = request.operation() + ":" + GsonUtils.getInstance().toJson(payload);
        long last = Objects.requireNonNullElse(target.getLastSequence(), 0L);
        if (sequence == last && Objects.equals(canonical, target.getLastPayload())) {
            // A lost response is answered from the committed result, including the terminal OFFLINE reply.
            return RegistrationFencingProtocol.Response.ok(new RegistrationFencingProtocol.MutationData(
                    String.valueOf(generation), String.valueOf(sequence), request.operation(), target.getLastResult()));
        }
        if (!ACTIVE.equals(target.getState())) {
            return error(RegistrationFencingProtocol.SESSION_RELEASED, "session has been terminated");
        }
        if (!Objects.equals(target.getGeneration(), generation)) {
            return error(RegistrationFencingProtocol.STALE_SESSION, "session generation has been superseded");
        }
        if (sequence == last) {
            return error(RegistrationFencingProtocol.SEQUENCE_CONFLICT, "sequence has another payload");
        }
        if (sequence < last) {
            return error(RegistrationFencingProtocol.OBSOLETE_OPERATION, "sequence is obsolete");
        }
        if (last == Long.MAX_VALUE || sequence > last + 1) {
            return error(RegistrationFencingProtocol.SEQUENCE_GAP, "sequence has a gap");
        }
        FencedRegisterService service = findRegisterService(target);
        if (Objects.isNull(service)) {
            return error(RegistrationFencingProtocol.FENCING_NOT_ENABLED, "registration writer is not guarded");
        }
        String selectorName = selectorName(target.getSelectorId());
        String result = applyFencedOperation(service, target, effectiveRequest, generation, selectorName);
        target.setLastSequence(sequence);
        target.setLastPayload(canonical);
        target.setLastResult(result);
        target.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        if (RegistrationFencingProtocol.OFFLINE.equals(request.operation())) {
            target.setGeneration(target.getGeneration() + 1L);
            target.setState(RELEASED);
            target.setOwnerStartupId(null);
        }
        targetMapper.update(target);
        return RegistrationFencingProtocol.Response.ok(new RegistrationFencingProtocol.MutationData(
                String.valueOf(generation), String.valueOf(sequence), request.operation(), result));
    }

    private String applyFencedOperation(final FencedRegisterService service,
                                        final RegistrationFencingTargetDO target,
                                        final RegistrationFencingProtocol.MutationRequest request,
                                        final long generation, final String selectorName) {
        try (RegistrationFencingContext ignored = RegistrationFencingContext.enter(
                target.getId(), target.getSelectorId(), target.getSessionId(), generation)) {
            List<URIRegisterDTO> payload = List.of(request.payload());
            return switch (request.operation()) {
                case RegistrationFencingProtocol.REGISTER -> service.registerURIWithoutFallback(
                        selectorName, payload, target.getNamespaceId());
                case RegistrationFencingProtocol.UPDATE -> EventType.DRAINING.equals(request.payload().getEventType())
                        ? service.drainingWithoutFallback(selectorName, payload, target.getNamespaceId())
                        : service.registerURIWithoutFallback(selectorName, payload, target.getNamespaceId());
                case RegistrationFencingProtocol.HEARTBEAT -> service.heartbeatWithoutFallback(
                        selectorName, payload, target.getNamespaceId());
                case RegistrationFencingProtocol.OFFLINE -> service.offlineWithoutFallback(
                        selectorName, payload, target.getNamespaceId());
                default -> throw new IllegalArgumentException("unsupported operation");
            };
        }
    }

    private RegistrationFencingTargetDO lockTargetWithSelector(final String targetId) {
        RegistrationFencingTargetDO lookup = targetMapper.selectById(targetId);
        if (Objects.isNull(lookup)) {
            return null;
        }
        selectorMapper.selectByIdForUpdate(lookup.getSelectorId());
        return targetMapper.selectByIdForUpdate(targetId);
    }

    private URIRegisterDTO canonicalPayload(final URIRegisterDTO payload) {
        try {
            URIRegisterDTO copy = GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(payload), URIRegisterDTO.class);
            copy.setHost(RegistrationFencingAddress.canonicalHost(copy.getHost()));
            copy.setProtocol(RegistrationFencingAddress.canonicalProtocol(copy.getProtocol()));
            return copy;
        } catch (IllegalArgumentException exception) {
            throw new RegistrationFencingException(RegistrationFencingProtocol.SOURCE_CONFLICT, exception.getMessage());
        }
    }

    private String canonicalEndpoint(final String endpoint) {
        try {
            return RegistrationFencingAddress.canonicalEndpoint(endpoint);
        } catch (IllegalArgumentException exception) {
            return endpoint;
        }
    }

    private String canonicalProtocol(final String protocol) {
        try {
            return RegistrationFencingAddress.canonicalProtocol(protocol);
        } catch (IllegalArgumentException exception) {
            return protocol;
        }
    }

    private String selectorName(final String selectorId) {
        SelectorDO selector = selectorMapper.selectById(selectorId);
        if (Objects.isNull(selector)) {
            throw new RegistrationFencingException(RegistrationFencingProtocol.SOURCE_CONFLICT, "selector does not exist");
        }
        return selector.getSelectorName();
    }

    private String payloadServiceName(final URIRegisterDTO payload) {
        if (StringUtils.isBlank(payload.getContextPath()) && StringUtils.isBlank(payload.getAppName())) {
            return null;
        }
        return ContextPathUtils.buildContextPath(payload.getContextPath(), payload.getAppName());
    }

    private FencedRegisterService findRegisterService(final RegistrationFencingTargetDO target) {
        PluginDO plugin = pluginMapper.selectById(target.getPluginId());
        if (Objects.isNull(plugin)) {
            return null;
        }
        return registerServices.values().stream()
                .filter(service -> Objects.equals(PluginNameAdapter.rpcTypeAdapter(service.rpcType()), plugin.getName()))
                .findFirst().orElse(null);
    }

    private void grant(final RegistrationFencingTargetDO target, final String startupId, final String principalId) {
        target.setGeneration(target.getGeneration() + 1L);
        target.setOwnerStartupId(startupId);
        target.setSessionId(UUID.randomUUID().toString());
        target.setLastSequence(0L);
        target.setLastPayload(null);
        target.setLastResult(null);
        target.setState(ACTIVE);
        target.setPrincipalId(principalId);
        target.setDateUpdated(new Timestamp(System.currentTimeMillis()));
    }

    private void validateTargetRequest(final RegistrationFencingProtocol.TargetRequest request) {
        if (Objects.isNull(request) || StringUtils.isBlank(request.domainId()) || StringUtils.isBlank(request.namespaceId())
                || StringUtils.isBlank(request.selectorId()) || StringUtils.isBlank(request.protocol())
                || StringUtils.isBlank(request.host()) || Objects.isNull(request.port()) || request.port() < 1 || request.port() > 65535
                || StringUtils.isBlank(request.principalId())) {
            throw new RegistrationFencingException(RegistrationFencingProtocol.INVALID_REQUEST, "invalid target request");
        }
    }

    private void requireEnabled() {
        if (!properties.isEnabled()) {
            throw new RegistrationFencingException(RegistrationFencingProtocol.FENCING_NOT_ENABLED, "fencing is not enabled");
        }
    }

    private boolean validOperation(final String operation) {
        return RegistrationFencingProtocol.REGISTER.equals(operation)
                || RegistrationFencingProtocol.UPDATE.equals(operation)
                || RegistrationFencingProtocol.HEARTBEAT.equals(operation)
                || RegistrationFencingProtocol.OFFLINE.equals(operation);
    }

    private boolean isUuid(final String value) {
        if (StringUtils.isBlank(value)) {
            return false;
        }
        try {
            return UUID.fromString(value).toString().equals(value);
        } catch (IllegalArgumentException ex) {
            return false;
        }
    }

    private long decimal(final String value) {
        if (StringUtils.isBlank(value) || (value.length() > 1 && value.startsWith("0"))) {
            throw new RegistrationFencingException(RegistrationFencingProtocol.INVALID_REQUEST, "invalid decimal value");
        }
        try {
            long parsed = Long.parseLong(value);
            if (parsed < 0) {
                throw new NumberFormatException();
            }
            return parsed;
        } catch (NumberFormatException ex) {
            throw new RegistrationFencingException(RegistrationFencingProtocol.INVALID_REQUEST, "invalid decimal value");
        }
    }

    private String nullToEmpty(final String value) {
        return StringUtils.defaultString(value);
    }

    private RegistrationFencingProtocol.TargetData toTargetData(final RegistrationFencingTargetDO target) {
        return new RegistrationFencingProtocol.TargetData(target.getId(), target.getDomainId(), target.getNamespaceId(),
                target.getSelectorId(), target.getProtocol(), target.getHost(), target.getPort(),
                String.valueOf(target.getGeneration()), target.getState());
    }

    private RegistrationFencingProtocol.SessionData toSessionData(final RegistrationFencingTargetDO target) {
        return new RegistrationFencingProtocol.SessionData(target.getDomainId(), target.getId(), target.getNamespaceId(), target.getSelectorId(), target.getSessionId(),
                target.getOwnerStartupId(), String.valueOf(target.getGeneration()), target.getState(),
                String.valueOf(Objects.requireNonNullElse(target.getLastSequence(), 0L)), target.getHost(), target.getPort(),
                target.getProtocol());
    }

    private <T> RegistrationFencingProtocol.Response<T> error(final String code, final String message) {
        return RegistrationFencingProtocol.Response.error(code, message);
    }
}
