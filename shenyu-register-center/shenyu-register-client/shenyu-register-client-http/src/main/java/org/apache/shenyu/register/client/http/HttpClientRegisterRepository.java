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

package org.apache.shenyu.register.client.http;

import com.github.benmanes.caffeine.cache.CacheLoader;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import okhttp3.Headers;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.AesUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.api.FailbackRegistryRepository;
import org.apache.shenyu.register.common.utils.OkHttpTools;
import org.apache.shenyu.register.common.utils.RegisterUtils;
import org.apache.shenyu.register.client.http.utils.RuntimeUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.protocol.RegistrationFencingProtocol;
import org.apache.shenyu.register.common.protocol.RegistrationFencingAddress;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
import org.apache.shenyu.spi.Join;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * The type Http client register repository.
 */
@Join
public class HttpClientRegisterRepository extends FailbackRegistryRepository {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpClientRegisterRepository.class);

    private static final String FENCING_DOMAINS_PROPERTY = "registrationFencingDomains";

    private static final String FENCING_STARTUP_ID_PROPERTY = "registrationFencingStartupId";

    private static final String FENCING_EXPECTED_GENERATION_PROPERTY = "registrationFencingExpectedGeneration";

    private static final String FENCING_DOMAIN_SEPARATOR = ";";

    private static final String FENCING_FIELD_SEPARATOR = "\\|";

    private final Map<String, URIRegisterDTO> uriRegisterDTOs = new ConcurrentHashMap<>();

    private final Map<String, ApiDocRegisterDTO> apiDocRegisterDTOs = new ConcurrentHashMap<>();

    private final Map<String, FencedDomain> fencedDomains = new LinkedHashMap<>();

    private final Map<String, FencedSession> fencedSessions = new ConcurrentHashMap<>();

    private final Map<String, FencedOperation> fencedOperations = new ConcurrentHashMap<>();

    private final Map<String, String> completedFencedRegistrations = new ConcurrentHashMap<>();

    private final Object fencedLock = new Object();

    private String startupId = UUID.randomUUID().toString();

    private String username;
    
    private String password;
    
    private List<String> serverList;
    
    /**
     * server -> accessToken.
     */
    private LoadingCache<String, String> accessToken;
    
    /**
     * Instantiates a new Http client register repository.
     */
    public HttpClientRegisterRepository() {
    }
    
    /**
     * Instantiates a new Http client register repository.
     *
     * @param config the config
     */
    public HttpClientRegisterRepository(final ShenyuRegisterCenterConfig config) {
        init(config);
    }
    
    @Override
    public void init(final ShenyuRegisterCenterConfig config) {
        this.username = config.getProps().getProperty(Constants.USER_NAME);
        this.password = config.getProps().getProperty(Constants.PASS_WORD);
        String secretKey = config.getProps().getProperty(Constants.AES_SECRET_KEY);
        String secretIv = config.getProps().getProperty(Constants.AES_SECRET_IV);
        if (StringUtils.isNotBlank(secretKey) && StringUtils.isNotBlank(secretIv)) {
            this.password = AesUtils.cbcEncrypt(secretKey, secretIv, password);
        }
        this.serverList = Lists.newArrayList(Splitter.on(",").split(config.getServerLists()));
        this.startupId = config.getProps().getProperty(FENCING_STARTUP_ID_PROPERTY, startupId).trim();
        validateUuid(this.startupId, FENCING_STARTUP_ID_PROPERTY);
        String defaultGeneration = config.getProps().getProperty(FENCING_EXPECTED_GENERATION_PROPERTY, "0").trim();
        validateGeneration(defaultGeneration, FENCING_EXPECTED_GENERATION_PROPERTY);
        initFencedDomains(config);
        this.accessToken = Caffeine.newBuilder()
                //see org.apache.shenyu.admin.config.properties.JwtProperties#expiredSeconds
                .expireAfterWrite(24L, TimeUnit.HOURS)
                .build(new CacheLoader<>() {
                    @Override
                    public @Nullable String load(@NonNull final String server) {
                        try {
                            Optional<?> login = RegisterUtils.doLogin(username, password, server.concat(Constants.LOGIN_PATH));
                            return login.map(String::valueOf).orElse(null);
                        } catch (Exception e) {
                            LOGGER.error("Login admin url :{} is fail, will retry. cause: {} ", server, e.getMessage());
                            return null;
                        }
                    }
                });
    }
    
    /**
     * Persist uri.
     *
     * @param registerDTO the register dto
     */
    @Override
    public void doPersistURI(final URIRegisterDTO registerDTO) {
        if (RuntimeUtils.listenByOther(registerDTO.getPort())) {
            return;
        }
        if (fencingEnabled()) {
            uriRegisterDTOs.put(uriIdentity(registerDTO), registerDTO);
            fencedMutate(registerDTO, RegistrationFencingProtocol.REGISTER);
            return;
        }
        doRegister(registerDTO, Constants.URI_PATH, Constants.URI);
        uriRegisterDTOs.put(uriIdentity(registerDTO), registerDTO);
    }
    
    @Override
    public void offline(final URIRegisterDTO offlineDTO) {
        if (fencingEnabled()) {
            offlineDTO.setEventType(EventType.OFFLINE);
            fencedMutate(offlineDTO, RegistrationFencingProtocol.OFFLINE);
            return;
        }
        doUnregister(offlineDTO);
    }

    @Override
    public void draining(final URIRegisterDTO drainingDTO) {
        if (RuntimeUtils.listenByOther(drainingDTO.getPort())) {
            return;
        }
        drainingDTO.setEventType(EventType.DRAINING);
        if (fencingEnabled()) {
            fencedMutate(drainingDTO, RegistrationFencingProtocol.UPDATE);
            return;
        }
        doRegister(drainingDTO, Constants.URI_PATH, Constants.URI);
    }

    @Override
    public void notReady(final URIRegisterDTO notReadyDTO) {
        if (RuntimeUtils.listenByOther(notReadyDTO.getPort())) {
            return;
        }
        notReadyDTO.setEventType(EventType.NOT_READY);
        if (fencingEnabled()) {
            fencedMutate(notReadyDTO, RegistrationFencingProtocol.UPDATE);
            return;
        }
        doRegister(notReadyDTO, Constants.URI_PATH, Constants.URI);
    }

    @Override
    public void ready(final URIRegisterDTO readyDTO) {
        if (RuntimeUtils.listenByOther(readyDTO.getPort())) {
            return;
        }
        readyDTO.setEventType(EventType.READY);
        if (fencingEnabled()) {
            fencedMutate(readyDTO, RegistrationFencingProtocol.UPDATE);
            return;
        }
        doRegister(readyDTO, Constants.URI_PATH, Constants.URI);
    }
    
    @Override
    public void sendHeartbeat(final URIRegisterDTO heartbeatDTO) {
        if (RuntimeUtils.listenByOther(heartbeatDTO.getPort())) {
            return;
        }
        heartbeatDTO.setEventType(EventType.HEARTBEAT);
        if (fencingEnabled()) {
            fencedMutate(heartbeatDTO, RegistrationFencingProtocol.HEARTBEAT);
            return;
        }
        doHeartbeat(heartbeatDTO, Constants.URI_PATH);
    }

    @Override
    public void sendHeartbeat(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        doHeartbeat(instanceBeatInfoDTO, Constants.BEAT_URI_PATH);
    }

    /**
     * doPersistApiDoc.
     *
     * @param registerDTO registerDTO
     */
    @Override
    protected void doPersistApiDoc(final ApiDocRegisterDTO registerDTO) {
        doRegister(registerDTO, Constants.API_DOC_PATH, Constants.API_DOC_TYPE);
        apiDocRegisterDTOs.put(apiDocIdentity(registerDTO), registerDTO);
    }
    
    @Override
    public void doPersistInterface(final MetaDataRegisterDTO metadata) {
        if (fencingEnabled()) {
            // A fenced selector is provisioned by the operator; the boot session owns only instance membership.
            LOGGER.debug("Skip metadata registration for a boot-session fenced client: {}", metadata.getPath());
            return;
        }
        doRegister(metadata, Constants.META_PATH, Constants.META_TYPE);
    }

    @Override
    protected void doPersistMcpTools(final McpToolsRegisterDTO registerDTO) {
        if (fencingEnabled()) {
            LOGGER.debug("Skip MCP tools registration for a boot-session fenced client");
            return;
        }
        doRegister(registerDTO, Constants.MCP_TOOLS_PATH, Constants.MCP_TOOLS_TYPE);
    }

    @Override
    public void closeRepository() {
        uriRegisterDTOs.values().forEach(registerDTO -> {
            if (fencingEnabled()) {
                registerDTO.setEventType(EventType.OFFLINE);
                try {
                    fencedMutate(registerDTO, RegistrationFencingProtocol.OFFLINE);
                } catch (RuntimeException ex) {
                    LOGGER.warn("Fenced offline failed for {}", registerDTO.getHost(), ex);
                }
                return;
            }
            registerDTO.setEventType(EventType.DELETED);
            doRegister(registerDTO, Constants.URI_PATH, Constants.URI);
        });
        apiDocRegisterDTOs.values().forEach(registerDTO -> {
            registerDTO.setEventType(EventType.OFFLINE);
            doRegister(registerDTO, Constants.API_DOC_PATH, Constants.API_DOC_TYPE);
        });
    }

    private static String uriIdentity(final URIRegisterDTO registerDTO) {
        return String.join(":", value(registerDTO.getNamespaceId()), value(registerDTO.getProtocol()), value(registerDTO.getAppName()),
                value(registerDTO.getContextPath()), value(registerDTO.getRpcType()), value(registerDTO.getHost()), value(registerDTO.getPort()));
    }

    private static String serviceName(final URIRegisterDTO registerDTO) {
        if (StringUtils.isBlank(registerDTO.getContextPath()) && StringUtils.isBlank(registerDTO.getAppName())) {
            return null;
        }
        return ContextPathUtils.buildContextPath(registerDTO.getContextPath(), registerDTO.getAppName());
    }

    private static String apiDocIdentity(final ApiDocRegisterDTO registerDTO) {
        return String.join(":", value(registerDTO.getContextPath()), value(registerDTO.getApiPath()), value(registerDTO.getHttpMethod()),
                value(registerDTO.getRpcType()), value(registerDTO.getVersion()));
    }

    private static String value(final Object value) {
        return Objects.toString(value, "");
    }

    /**
     * doPersistDiscoveryConfig.
     *
     * @param discoveryConfigRegisterDTO discoveryConfigRegisterDTO
     */
    public void doPersistDiscoveryConfig(final DiscoveryConfigRegisterDTO discoveryConfigRegisterDTO) {
        if (fencingEnabled()) {
            LOGGER.debug("Skip discovery config registration for a boot-session fenced client");
            return;
        }
        doRegister(discoveryConfigRegisterDTO, Constants.DISCOVERY_CONFIG_PATH, Constants.DISCOVERY_CONFIG_TYPE);
    }

    private boolean fencingEnabled() {
        return !fencedDomains.isEmpty();
    }

    private void initFencedDomains(final ShenyuRegisterCenterConfig config) {
        String raw = config.getProps().getProperty(FENCING_DOMAINS_PROPERTY);
        if (StringUtils.isBlank(raw)) {
            return;
        }
        for (String entry : raw.split(FENCING_DOMAIN_SEPARATOR)) {
            if (StringUtils.isBlank(entry)) {
                continue;
            }
            String[] fields = entry.trim().split(FENCING_FIELD_SEPARATOR, -1);
            if (fields.length < 2 || StringUtils.isBlank(fields[0]) || StringUtils.isBlank(fields[1])) {
                throw new IllegalArgumentException("Invalid registrationFencingDomains entry: " + entry);
            }
            List<String> servers = fields.length >= 3 && StringUtils.isNotBlank(fields[2])
                    ? Lists.newArrayList(Splitter.on(",").split(fields[2]))
                    : new ArrayList<>(serverList);
            if (servers.isEmpty()) {
                throw new IllegalArgumentException("Registration fencing domain has no server: " + entry);
            }
            String domainStartupId = fields.length >= 4 && StringUtils.isNotBlank(fields[3]) ? fields[3].trim() : startupId;
            String domainGeneration = fields.length >= 5 && StringUtils.isNotBlank(fields[4])
                    ? fields[4].trim() : configGeneration(config);
            validateUuid(domainStartupId, FENCING_STARTUP_ID_PROPERTY);
            validateGeneration(domainGeneration, FENCING_EXPECTED_GENERATION_PROPERTY);
            String targetHost = fields.length >= 6 && StringUtils.isNotBlank(fields[5])
                    ? RegistrationFencingAddress.canonicalHost(fields[5].trim()) : null;
            Integer targetPort = fields.length >= 7 && StringUtils.isNotBlank(fields[6]) ? Integer.valueOf(fields[6].trim()) : null;
            String targetProtocol = fields.length >= 8 && StringUtils.isNotBlank(fields[7])
                    ? RegistrationFencingAddress.canonicalProtocol(fields[7].trim()) : null;
            if (Objects.isNull(targetHost) || Objects.isNull(targetPort) || Objects.isNull(targetProtocol)
                    || targetPort < 1 || targetPort > 65535) {
                throw new IllegalArgumentException("Invalid registration fencing address: " + entry);
            }
            String namespaceId = fields.length >= 9 && StringUtils.isNotBlank(fields[8]) ? fields[8].trim() : null;
            if (StringUtils.isBlank(namespaceId)) {
                throw new IllegalArgumentException("Registration fencing target requires a namespace id: " + entry);
            }
            String serviceName = fields.length >= 10 && StringUtils.isNotBlank(fields[9])
                    ? ContextPathUtils.buildContextPath(fields[9].trim(), null) : null;
            if (StringUtils.isBlank(serviceName)) {
                throw new IllegalArgumentException("Registration fencing target requires a selector service name: " + entry);
            }
            FencedDomain domain = new FencedDomain(fields[0].trim(), fields[1].trim(), servers, domainStartupId,
                    domainGeneration, targetHost, targetPort, targetProtocol, namespaceId, serviceName);
            if (Objects.nonNull(fencedDomains.put(domain.key(), domain))) {
                throw new IllegalArgumentException("Duplicate registration fencing target: " + domain.key());
            }
        }
    }

    /**
     * Apply one ordered, per-domain operation. All operations of one instance share a single
     * unconfirmed slot: a different operation first completes or retries the pending one, so the
     * strict client sequence never has a gap. A failed domain keeps its sequence for retry; a
     * domain that already committed is not sent the same operation again.
     *
     * @param dto instance payload
     * @param operation fencing operation
     */
    private void fencedMutate(final URIRegisterDTO dto, final String operation) {
        synchronized (fencedLock) {
            boolean matched = false;
            RuntimeException failure = null;
            String payload = GsonUtils.getInstance().toJson(dto);
            for (FencedDomain domain : fencedDomains.values()) {
                if (!domain.matches(dto)) {
                    continue;
                }
                matched = true;
                String key = uriIdentity(dto) + "|" + domain.key();
                if (RegistrationFencingProtocol.REGISTER.equals(operation)
                        && Objects.equals(completedFencedRegistrations.get(key), payload)) {
                    continue;
                }
                FencedOperation pending = fencedOperations.get(key);
                try {
                    if (Objects.nonNull(pending)
                            && !(pending.operation.equals(operation) && pending.payload.equals(payload))) {
                        applyPending(key, pending);
                    }
                    pending = fencedOperations.computeIfAbsent(key,
                            ignored -> new FencedOperation(domain.key(), operation, payload));
                    applyPending(key, pending);
                } catch (RuntimeException ex) {
                    if (Objects.isNull(failure)) {
                        failure = ex;
                    } else {
                        failure.addSuppressed(ex);
                    }
                }
            }
            if (!matched) {
                throw new FencedRegistrationException(RegistrationFencingProtocol.SOURCE_CONFLICT,
                        "no configured fencing target matches the URI address");
            }
            if (Objects.nonNull(failure)) {
                throw failure;
            }
        }
    }

    private void applyPending(final String key, final FencedOperation operation) {
        synchronized (operation) {
            RuntimeException failure = null;
            boolean matched = false;
            for (FencedDomain domain : fencedDomains.values()) {
                if (!Objects.equals(domain.key(), operation.domainKey)) {
                    continue;
                }
                if (operation.completed.contains(domain.key())) {
                    continue;
                }
                try {
                    boolean applied = applyFenced(domain, operation);
                    matched |= applied;
                    if (applied) {
                        operation.completed.add(domain.key());
                    }
                } catch (RuntimeException ex) {
                    if (Objects.isNull(failure)) {
                        failure = ex;
                    } else {
                        failure.addSuppressed(ex);
                    }
                }
            }
            if (Objects.nonNull(failure)) {
                throw failure;
            }
            if (!matched && operation.completed.isEmpty()) {
                throw new FencedRegistrationException(RegistrationFencingProtocol.SOURCE_CONFLICT,
                        "no configured fencing target matches the URI address");
            }
            fencedOperations.remove(key, operation);
            if (RegistrationFencingProtocol.REGISTER.equals(operation.operation)) {
                completedFencedRegistrations.put(key, operation.payload);
            } else if (RegistrationFencingProtocol.OFFLINE.equals(operation.operation)) {
                completedFencedRegistrations.remove(key);
            }
        }
    }

    private boolean applyFenced(final FencedDomain domain, final FencedOperation operation) {
        URIRegisterDTO payload = operation.payload();
        if (!domain.matches(payload)) {
            return false;
        }
        FencedSession session = acquireFencedSession(domain);
        synchronized (session) {
            if (!session.matches(payload)) {
                return false;
            }
            Long sequence = operation.sequences.get(domain.key());
            if (Objects.isNull(sequence)) {
                sequence = session.lastSequence + 1;
                operation.sequences.put(domain.key(), sequence);
            }
            RegistrationFencingProtocol.MutationRequest request = new RegistrationFencingProtocol.MutationRequest(
                    domain.domainId, session.sessionId, session.selectorId, String.valueOf(session.generation),
                    String.valueOf(sequence), operation.operation, payload);
            Map<String, Object> response = postFenced(domain,
                    RegistrationFencingProtocol.MUTATE_PATH.replace("{sessionId}", session.sessionId), request);
            String code = responseCode(response);
            if (RegistrationFencingProtocol.OK.equals(code)) {
                session.lastSequence = sequence;
                return true;
            }
            if (terminalCode(code)) {
                operation.sequences.remove(domain.key());
                resetFencedSession(session);
            }
            throw new FencedRegistrationException(code, responseMessage(response));
        }
    }

    private FencedSession acquireFencedSession(final FencedDomain domain) {
        FencedSession session = fencedSessions.computeIfAbsent(domain.key(), ignored -> new FencedSession(domain.expectedGeneration));
        synchronized (session) {
            if (StringUtils.isNotBlank(session.sessionId)) {
                return session;
            }
            RegistrationFencingProtocol.AcquireRequest request = new RegistrationFencingProtocol.AcquireRequest(
                    domain.domainId, domain.targetId, domain.startupId, session.expectedGeneration);
            Map<String, Object> response = postFenced(domain, RegistrationFencingProtocol.ACQUIRE_PATH, request);
            String code = responseCode(response);
            if (!RegistrationFencingProtocol.OK.equals(code)) {
                throw new FencedRegistrationException(code, responseMessage(response));
            }
            Map<String, Object> data = responseData(response);
            String sessionId = Objects.toString(data.get("sessionId"), null);
            if (StringUtils.isBlank(sessionId)) {
                throw new FencedRegistrationException(RegistrationFencingProtocol.INVALID_REQUEST,
                        "Admin did not return a registration session");
            }
            session.sessionId = sessionId;
            session.generation = toLong(data.get("generation"));
            session.lastSequence = toLong(data.get("lastSequence"));
            session.expectedGeneration = String.valueOf(session.generation);
            session.targetHost = Objects.toString(data.get("host"), null);
            session.targetPort = toInteger(data.get("port"));
            session.targetProtocol = Objects.toString(data.get("protocol"), null);
            session.namespaceId = Objects.toString(data.get("namespaceId"), null);
            session.selectorId = Objects.toString(data.get("selectorId"), null);
            return session;
        }
    }

    private void resetFencedSession(final FencedSession session) {
        session.sessionId = null;
        session.lastSequence = 0L;
        session.targetHost = null;
        session.targetPort = null;
        session.targetProtocol = null;
        session.namespaceId = null;
        session.selectorId = null;
    }

    private Map<String, Object> postFenced(final FencedDomain domain, final String path, final Object request) {
        RuntimeException failure = null;
        for (String server : domain.servers) {
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                Headers headers = new Headers.Builder()
                        .add(RegistrationProtocol.VERSION_HEADER, RegistrationProtocol.V1)
                        .add(Constants.X_ACCESS_TOKEN, accessToken)
                        .build();
                String body = OkHttpTools.getInstance().post(server.concat(path),
                        GsonUtils.getInstance().toJson(request), headers);
                Map<String, Object> response = GsonUtils.getInstance().convertToMap(body);
                if (Objects.isNull(response) || StringUtils.isBlank(responseCode(response))) {
                    throw new IOException("Invalid fencing response from " + server);
                }
                return response;
            } catch (Exception ex) {
                LOGGER.error("Fencing request to {} is fail, will retry. cause: {}", server, ex.getMessage());
                if (Objects.isNull(failure)) {
                    failure = new RuntimeException(ex);
                } else {
                    failure.addSuppressed(ex);
                }
            }
        }
        throw Objects.requireNonNull(failure);
    }

    private String responseCode(final Map<String, Object> response) {
        return Objects.toString(response.get("code"), null);
    }

    private String responseMessage(final Map<String, Object> response) {
        return Objects.toString(response.get("message"), null);
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> responseData(final Map<String, Object> response) {
        Object data = response.get("data");
        if (data instanceof Map) {
            return (Map<String, Object>) data;
        }
        throw new FencedRegistrationException(RegistrationFencingProtocol.INVALID_REQUEST,
                "Admin returned no fencing data");
    }

    private boolean terminalCode(final String code) {
        return RegistrationFencingProtocol.STALE_SESSION.equals(code)
                || RegistrationFencingProtocol.SUPERSEDED.equals(code)
                || RegistrationFencingProtocol.SESSION_RELEASED.equals(code)
                || RegistrationFencingProtocol.OWNER_CONFLICT.equals(code);
    }

    private long toLong(final Object value) {
        try {
            return Long.parseLong(Objects.toString(value, "0"));
        } catch (NumberFormatException ex) {
            return 0L;
        }
    }

    private Integer toInteger(final Object value) {
        if (Objects.isNull(value)) {
            return null;
        }
        try {
            return Integer.valueOf(Objects.toString(value));
        } catch (NumberFormatException ex) {
            return null;
        }
    }

    private String configGeneration(final ShenyuRegisterCenterConfig config) {
        return config.getProps().getProperty(FENCING_EXPECTED_GENERATION_PROPERTY, "0").trim();
    }

    private void validateUuid(final String value, final String property) {
        try {
            if (!UUID.fromString(value).toString().equals(value)) {
                throw new IllegalArgumentException();
            }
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException(property + " must be a canonical UUID", ex);
        }
    }

    private void validateGeneration(final String value, final String property) {
        try {
            if (value.isEmpty() || value.length() > 1 && value.startsWith("0") || Long.parseLong(value) < 0) {
                throw new IllegalArgumentException();
            }
        } catch (IllegalArgumentException ex) {
            throw new IllegalArgumentException(property + " must be a non-negative decimal", ex);
        }
    }

    private <T> void doRegister(final T t, final String path, final String type) {
        RuntimeException failure = null;
        for (String server : serverList) {
            String concat = server.concat(path);
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                RegisterUtils.doRegister(GsonUtils.getInstance().toJson(t), concat, type, accessToken);
                // considering the situation of multiple clusters, we should continue to execute here
            } catch (Exception e) {
                LOGGER.error("Register admin url :{} is fail, will retry. cause:{}", server, e.getMessage());
                if (Objects.isNull(failure)) {
                    failure = new RuntimeException(e);
                } else {
                    failure.addSuppressed(e);
                }
            }
        }
        if (Objects.nonNull(failure)) {
            // Failback replays the registration to every server, so admin registration endpoints must be idempotent.
            throw failure;
        }
    }

    private <T> void doHeartbeat(final T t, final String path) {
        RuntimeException failure = null;
        for (String server : serverList) {
            String concat = server.concat(path);
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                RegisterUtils.doHeartBeat(GsonUtils.getInstance().toJson(t), concat, Constants.HEARTBEAT, accessToken);
            } catch (Exception e) {
                LOGGER.error("HeartBeat admin url :{} is fail, will retry.", server, e);
                if (Objects.isNull(failure)) {
                    failure = new RuntimeException(e);
                } else {
                    failure.addSuppressed(e);
                }
            }
        }
        if (Objects.nonNull(failure)) {
            throw failure;
        }
    }
    
    private <T> void doUnregister(final T t) {
        int failureCount = 0;
        for (String server : serverList) {
            String concat = server.concat(Constants.OFFLINE_PATH);
            try {
                String accessToken = this.accessToken.get(server);
                if (StringUtils.isBlank(accessToken)) {
                    throw new NullPointerException("accessToken is null");
                }
                RegisterUtils.doUnregister(GsonUtils.getInstance().toJson(t), concat, accessToken);
                // considering the situation of multiple clusters, we should continue to execute here
            } catch (Exception e) {
                failureCount++;
                LOGGER.error("Unregister admin url :{} is fail.", server, e);
                if (failureCount == serverList.size()) {
                    throw new RuntimeException(e);
                }
            }
        }
    }

    /**
     * One explicitly configured registration authority domain.
     */
    private static final class FencedDomain {

        private final String domainId;

        private final String targetId;

        private final List<String> servers;

        private final String startupId;

        private final String expectedGeneration;

        private final String targetHost;

        private final Integer targetPort;

        private final String targetProtocol;

        private final String namespaceId;

        private final String serviceName;

        private FencedDomain(final String domainId, final String targetId, final List<String> servers,
                             final String startupId, final String expectedGeneration, final String targetHost,
                             final Integer targetPort, final String targetProtocol, final String namespaceId,
                             final String serviceName) {
            this.domainId = domainId;
            this.targetId = targetId;
            this.servers = servers;
            this.startupId = startupId;
            this.expectedGeneration = expectedGeneration;
            this.targetHost = targetHost;
            this.targetPort = targetPort;
            this.targetProtocol = targetProtocol;
            this.namespaceId = namespaceId;
            this.serviceName = serviceName;
        }

        private String key() {
            return domainId + "|" + targetId;
        }

        private boolean matches(final URIRegisterDTO dto) {
            if (StringUtils.isBlank(targetHost)) {
                return true;
            }
            try {
                return Objects.equals(namespaceId, dto.getNamespaceId())
                        && Objects.equals(serviceName, serviceName(dto))
                        && Objects.equals(targetHost, RegistrationFencingAddress.canonicalHost(dto.getHost()))
                        && Objects.equals(targetPort, dto.getPort())
                        && Objects.equals(targetProtocol, RegistrationFencingAddress.canonicalProtocol(dto.getProtocol()));
            } catch (IllegalArgumentException exception) {
                return false;
            }
        }
    }

    /**
     * Per-domain boot session and committed sequence state.
     */
    private static final class FencedSession {

        private String sessionId;

        private long generation;

        private long lastSequence;

        private String expectedGeneration;

        private String targetHost;

        private Integer targetPort;

        private String targetProtocol;

        private String namespaceId;

        private String selectorId;

        private FencedSession(final String expectedGeneration) {
            this.expectedGeneration = expectedGeneration;
        }

        private boolean matches(final URIRegisterDTO dto) {
            try {
                return StringUtils.isBlank(namespaceId) || !Objects.equals(namespaceId, dto.getNamespaceId())
                        ? false : StringUtils.isBlank(targetHost) || Objects.isNull(targetPort) || StringUtils.isBlank(targetProtocol)
                        || (Objects.equals(targetHost, RegistrationFencingAddress.canonicalHost(dto.getHost()))
                        && Objects.equals(namespaceId, dto.getNamespaceId())
                        && Objects.equals(targetPort, dto.getPort())
                        && Objects.equals(targetProtocol, RegistrationFencingAddress.canonicalProtocol(dto.getProtocol())));
            } catch (IllegalArgumentException exception) {
                return false;
            }
        }
    }

    /**
     * One unconfirmed operation with per-domain sequence and completion tracking.
     */
    private static final class FencedOperation {

        private final String domainKey;

        private final String operation;

        private final String payload;

        private final Map<String, Long> sequences = new HashMap<>();

        private final Set<String> completed = new HashSet<>();

        private FencedOperation(final String domainKey, final String operation, final String payload) {
            this.domainKey = domainKey;
            this.operation = operation;
            this.payload = payload;
        }

        private URIRegisterDTO payload() {
            return GsonUtils.getInstance().fromJson(payload, URIRegisterDTO.class);
        }
    }

    /**
     * Stable fencing failure reported by Admin, not retried as a legacy registration.
     */
    private static final class FencedRegistrationException extends RuntimeException {

        private FencedRegistrationException(final String code, final String message) {
            super(code + ": " + message);
        }
    }
}
