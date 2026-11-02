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

package org.apache.shenyu.admin.service.register;

import jakarta.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingContext;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.manager.RegisterApiDocService;
import org.apache.shenyu.admin.service.publish.InstanceInfoReportEventPublisher;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.protocol.RegistrationFencingAddress;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.function.Supplier;
import java.util.function.BiConsumer;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * Abstract strategy.
 */
public abstract class AbstractShenyuClientRegisterServiceImpl extends FallbackShenyuClientRegisterService
        implements ShenyuClientRegisterService, FencedRegisterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuClientRegisterServiceImpl.class);
    
    /**
     * The Event publisher.
     */
    @Resource
    private ApplicationEventPublisher eventPublisher;

    /**
     * The Selector service.
     */
    @Resource
    private SelectorService selectorService;

    @Resource
    private MetaDataService metaDataService;

    /**
     * The Rule service.
     */
    @Resource
    private RuleService ruleService;

    @Resource
    private UpstreamCheckService upstreamCheckService;

    @Resource
    private RegisterApiDocService registerApiDocService;

    @Resource
    private DiscoveryService discoveryService;

    @Resource
    private DiscoveryUpstreamService discoveryUpstreamService;

    @Resource
    private PluginMapper pluginMapper;

    @Resource
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Resource
    private InstanceInfoReportEventPublisher publisher;

    @Resource
    private RegistrationFencingGuard registrationFencingGuard;

    /**
     * Registration writes need one database transaction so the selector row
     * lock held by the fencing guard covers the subsequent handle and
     * discovery updates. The field is optional in unit tests that instantiate
     * the service without a Spring transaction manager.
     */
    @Resource
    private PlatformTransactionManager transactionManager;

    /**
     * Selector handler string.
     *
     * @param metaDataDTO the meta data dto
     * @return the string
     */
    protected abstract String selectorHandler(MetaDataRegisterDTO metaDataDTO);

    /**
     * Rule handler string.
     *
     * @return the string
     */
    protected abstract String ruleHandler();

    /**
     * Register metadata.
     *
     * @param metaDataDTO the meta data dto
     */
    protected abstract void registerMetadata(MetaDataRegisterDTO metaDataDTO);

    /**
     * Build handle string.
     *
     * @param uriList    the uri list
     * @param selectorDO the selector do
     * @return the string
     */
    protected abstract String buildHandle(List<URIRegisterDTO> uriList, SelectorDO selectorDO);

    /**
     * Register meta data.
     *
     * @param dto meta data register dto.
     * @return the string
     */
    @Override
    public String register(final MetaDataRegisterDTO dto) {
        String namespaceId = StringUtils.defaultIfEmpty(dto.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID);
        String pluginName = PluginNameAdapter.rpcTypeAdapter(rpcType());
        this.checkNamespacePluginRel(namespaceId, pluginName);
        // Shared service metadata may not rewrite a selector that a boot session protects.
        if (StringUtils.isNotBlank(dto.getContextPath()) || StringUtils.isNotBlank(dto.getAppName())) {
            registrationFencingGuard.assertMetadataWriteAllowed(
                    ContextPathUtils.buildContextPath(dto.getContextPath(), dto.getAppName()), pluginName, namespaceId);
        }
        dto.setNamespaceId(namespaceId);
        //handler plugin selector
        String selectorHandler = selectorHandler(dto);
        String selectorId = selectorService.registerDefault(dto, PluginNameAdapter.rpcTypeAdapter(rpcType()), selectorHandler);
        //handler selector rule
        String ruleHandler = ruleHandler();
        RuleDTO ruleDTO = buildRpcDefaultRuleDTO(selectorId, dto, ruleHandler);
        ruleService.registerDefault(ruleDTO);
        //handler register metadata
        registerMetadata(dto);
        //handler context path
        String contextPath = dto.getContextPath();
        if (StringUtils.isNotEmpty(contextPath)) {
            registerContextPath(dto);
        }
        return ShenyuResultMessage.SUCCESS;
    }

    @Override
    public String registerApiDoc(final ApiDocRegisterDTO apiDocRegisterDTO) {
        registerApiDocService.registerApiDocument(apiDocRegisterDTO);
        return ShenyuResultMessage.SUCCESS;
    }

    /**
     * Register uri string.
     *
     * @param selectorName the selector name
     * @param uriList      the uri list
     * @param namespaceId  the namespace id
     * @return the string
     */
    @Override
    public String doRegisterURI(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return executeRegistrationTransaction(() -> doRegisterURIInTransaction(selectorName, uriList, namespaceId));
    }

    private String doRegisterURIInTransaction(final String selectorName, final List<URIRegisterDTO> uriList,
                                              final String namespaceId) {
        if (CollectionUtils.isEmpty(uriList)) {
            return "";
        }
        String pluginName = PluginNameAdapter.rpcTypeAdapter(rpcType());
        SelectorDO selectorDO = selectorService.findByNameAndPluginNameAndNamespaceIdForUpdate(selectorName, pluginName, namespaceId);
        LOG.info("selector info params: selectorName: {}, pluginName: {}, namespaceId: {}, selectorDO info: {}.",
                selectorName, pluginName, namespaceId, selectorDO);
        if (Objects.isNull(selectorDO)) {
            throw new ShenyuException("doRegister Failed to execute, because selectorDO object is null, wait to retry.");
        }
        this.checkNamespacePluginRel(namespaceId, pluginName);
        // fetch UPSTREAM_MAP data from db
        //upstreamCheckService.fetchUpstreamData();
        //update upstream
        List<URIRegisterDTO> validUriList = uriList.stream()
                .filter(dto -> Objects.nonNull(dto.getPort()) && StringUtils.isNotBlank(dto.getHost()))
                .collect(Collectors.toList());
        validUriList.forEach(uri -> registrationFencingGuard.assertLegacyUriWrite(selectorDO.getId(), uri.getHost(), uri.getPort()));
        String handler = buildHandle(validUriList, selectorDO);
        if (Objects.nonNull(handler)) {
            selectorDO.setHandle(handler);
            SelectorData selectorData = selectorService.buildByNameAndPluginNameAndNamespaceId(selectorName, PluginNameAdapter.rpcTypeAdapter(rpcType()), namespaceId);
            selectorData.setHandle(handler);
            // update db
            selectorService.updateSelectiveForRegistration(selectorDO);
            // publish change event.
        }
        // A new fenced session must re-claim an existing address and clear a prior drain,
        // even when the selector handle is otherwise unchanged.
        doDiscoveryLocal(selectorDO, pluginName, validUriList);
        return ShenyuResultMessage.SUCCESS;
    }

    @Override
    public void checkNamespacePluginRel(final String namespaceId, final String pluginName) {
        String errorMsg = String.format("%s plugin not enabled for current namespace or plugin not exist for namespaceId: %s", pluginName, namespaceId);
        PluginDO pluginDO = pluginMapper.selectByName(pluginName);
        if (Objects.isNull(pluginDO)) {
            throw new IllegalArgumentException(errorMsg);
        }
        NamespacePluginVO namespacePluginRelation = namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginDO.getId(), namespaceId);
        if (Objects.isNull(namespacePluginRelation)) {
            throw new IllegalArgumentException(errorMsg);
        }
    }
    
    @Override
    public String doHeartbeat(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return executeRegistrationTransaction(() -> doHeartbeatInTransaction(selectorName, uriList, namespaceId));
    }

    private String doHeartbeatInTransaction(final String selectorName, final List<URIRegisterDTO> uriList,
                                            final String namespaceId) {
        if (CollectionUtils.isEmpty(uriList)) {
            return "";
        }
        String pluginName = PluginNameAdapter.rpcTypeAdapter(rpcType());
        SelectorDO selectorDO = selectorService.findByNameAndPluginNameAndNamespaceIdForUpdate(selectorName, pluginName, namespaceId);
        if (Objects.isNull(selectorDO)) {
            throw new ShenyuException("doHeartbeat Failed to execute,wait to retry.");
        }
        // update upstream
        List<URIRegisterDTO> validUriList = uriList.stream().filter(dto -> Objects.nonNull(dto.getPort()) && StringUtils.isNotBlank(dto.getHost())).toList();
        if (CollectionUtils.isEmpty(validUriList)) {
            return null;
        }
        // discovery publish change event.
        String selectorId = selectorDO.getId();
        validUriList.forEach(uri -> registrationFencingGuard.assertLegacyUriWrite(selectorId, uri.getHost(), uri.getPort()));
        // change live node status to TRUE
        validUriList.forEach(uriRegisterDTO -> {
            DiscoveryUpstreamDTO discoveryUpstreamDTO = CommonUpstreamUtils.buildDefaultDiscoveryUpstreamDTO(uriRegisterDTO.getHost(),
                    uriRegisterDTO.getPort(),
                    uriRegisterDTO.getProtocol(),
                    uriRegisterDTO.getNamespaceId());

            try {
                // Fenced heartbeats do not update the address-only diagnostic stream.
                if (Objects.isNull(RegistrationFencingContext.current())) {
                    InstanceInfoReportEvent instanceInfoReportEvent = new InstanceInfoReportEvent(
                            uriRegisterDTO.getHost(),
                            String.valueOf(uriRegisterDTO.getPort()),
                            InstanceTypeConstants.CLIENT_INSTANCE_TYPE,
                            uriRegisterDTO.getInstanceInfo(),
                            uriRegisterDTO.getNamespaceId()
                    );
                    publisher.publish(instanceInfoReportEvent);
                }
            } catch (Exception e) {
                LOG.error("publish instance info error", e);
            }

            // Local probe mode: bootstrap-local health checking is the authority, so a plain heartbeat
            // only reports instance info and triggers the snapshot refresh below; it must not write back
            // upstream status on the admin's behalf, in particular it must not revive an upstream that
            // the operator explicitly disabled.
            if (!upstreamCheckService.isLocalMode()) {
                LOG.info("change alive selectorId={}|url={}", selectorId, discoveryUpstreamDTO.getUrl());
                discoveryUpstreamService.changeStatusBySelectorIdAndUrl(selectorId, discoveryUpstreamDTO.getUrl(), Boolean.TRUE);
            }
        });

        DiscoverySyncData discoverySyncData = fetch(selectorId, selectorDO.getSelectorName(), pluginName, namespaceId);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.REFRESH, Collections.singletonList(discoverySyncData)));
        
        return ShenyuResultMessage.SUCCESS;
    }
    
    @Override
    public String doDraining(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return executeRegistrationTransaction(() -> doDrainingInTransaction(selectorName, uriList, namespaceId));
    }

    @Override
    public String doNotReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return updateAdmission(selectorName, uriList, namespaceId, "doNotReady",
                (selectorId, url) -> discoveryUpstreamService.markNotReadyBySelectorIdAndUrl(selectorId, url, true));
    }

    @Override
    public String doReady(final String selectorName, final List<URIRegisterDTO> uriList, final String namespaceId) {
        return updateAdmission(selectorName, uriList, namespaceId, "doReady", discoveryUpstreamService::markReadyBySelectorIdAndUrl);
    }

    private String updateAdmission(final String selectorName, final List<URIRegisterDTO> uriList,
                                   final String namespaceId, final String action,
                                   final BiConsumer<String, String> marker) {
        return executeRegistrationTransaction(() -> updateAdmissionInTransaction(selectorName, uriList, namespaceId, action, marker));
    }

    private String updateAdmissionInTransaction(final String selectorName, final List<URIRegisterDTO> uriList,
                                                final String namespaceId, final String action,
                                                final BiConsumer<String, String> marker) {
        if (CollectionUtils.isEmpty(uriList)) {
            return "";
        }
        String pluginName = PluginNameAdapter.rpcTypeAdapter(rpcType());
        SelectorDO selectorDO = selectorService.findByNameAndPluginNameAndNamespaceIdForUpdate(selectorName, pluginName, namespaceId);
        if (Objects.isNull(selectorDO)) {
            throw new ShenyuException(action + " Failed to execute, wait to retry.");
        }
        List<URIRegisterDTO> validUriList = uriList.stream().filter(dto -> Objects.nonNull(dto.getPort()) && StringUtils.isNotBlank(dto.getHost())).toList();
        if (CollectionUtils.isEmpty(validUriList)) {
            return null;
        }
        String selectorId = selectorDO.getId();
        assertLegacyUriWrite(selectorId, validUriList);
        validUriList.forEach(uriRegisterDTO -> {
            String url = CommonUpstreamUtils.buildDefaultDiscoveryUpstreamDTO(uriRegisterDTO.getHost(),
                    uriRegisterDTO.getPort(), uriRegisterDTO.getProtocol(), uriRegisterDTO.getNamespaceId()).getUrl();
            marker.accept(selectorId, url);
        });
        DiscoverySyncData discoverySyncData = fetch(selectorId, selectorDO.getSelectorName(), pluginName, namespaceId);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE,
                Collections.singletonList(discoverySyncData)));
        return ShenyuResultMessage.SUCCESS;
    }

    private String doDrainingInTransaction(final String selectorName, final List<URIRegisterDTO> uriList,
                                           final String namespaceId) {
        if (CollectionUtils.isEmpty(uriList)) {
            return "";
        }
        String pluginName = PluginNameAdapter.rpcTypeAdapter(rpcType());
        SelectorDO selectorDO = selectorService.findByNameAndPluginNameAndNamespaceIdForUpdate(selectorName, pluginName, namespaceId);
        if (Objects.isNull(selectorDO)) {
            throw new ShenyuException("doDraining Failed to execute, wait to retry.");
        }
        List<URIRegisterDTO> validUriList = uriList.stream().filter(dto -> Objects.nonNull(dto.getPort()) && StringUtils.isNotBlank(dto.getHost())).toList();
        if (CollectionUtils.isEmpty(validUriList)) {
            return null;
        }
        String selectorId = selectorDO.getId();
        validUriList.forEach(uri -> registrationFencingGuard.assertLegacyUriWrite(selectorId, uri.getHost(), uri.getPort()));
        // the marker travels in props, not in status: heartbeats keep flipping status
        // back to enabled, and a draining instance must stay excluded regardless (#8042)
        validUriList.forEach(uriRegisterDTO -> {
            String url = CommonUpstreamUtils.buildDefaultDiscoveryUpstreamDTO(uriRegisterDTO.getHost(),
                    uriRegisterDTO.getPort(), uriRegisterDTO.getProtocol(), uriRegisterDTO.getNamespaceId()).getUrl();
            discoveryUpstreamService.markDrainingBySelectorIdAndUrl(selectorId, url, true);
        });
        DiscoverySyncData discoverySyncData = fetch(selectorId, selectorDO.getSelectorName(), pluginName, namespaceId);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData)));
        return ShenyuResultMessage.SUCCESS;
    }

    protected void doDiscoveryLocal(final SelectorDO selectorDO, final String pluginName, final List<URIRegisterDTO> uriList) {
        String discoveryHandlerId = discoveryService.registerDefaultDiscovery(selectorDO.getId(), pluginName, selectorDO.getNamespaceId());
        for (URIRegisterDTO uriRegisterDTO : uriList) {
            DiscoveryUpstreamDTO discoveryUpstreamDTO = CommonUpstreamUtils.buildDefaultDiscoveryUpstreamDTO(uriRegisterDTO.getHost(),
                    uriRegisterDTO.getPort(), uriRegisterDTO.getProtocol(), selectorDO.getNamespaceId());
            discoveryUpstreamDTO.setDiscoveryHandlerId(discoveryHandlerId);
            discoveryUpstreamService.nativeCreateOrUpdate(discoveryUpstreamDTO);
            // a fresh registration claims the identity: a restart releases the drain,
            // while heartbeats alone can never do so (#8042)
            discoveryUpstreamService.markReadyBySelectorIdAndUrl(selectorDO.getId(), discoveryUpstreamDTO.getUrl());
        }
        DiscoverySyncData discoverySyncData = fetch(selectorDO.getId(), selectorDO.getSelectorName(), pluginName, selectorDO.getNamespaceId());
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, DataEventTypeEnum.UPDATE, Collections.singletonList(discoverySyncData)));
    }

    protected DiscoverySyncData fetch(final String selectorId, final String selectorName, final String pluginName, final String namespaceId) {
        List<DiscoveryUpstreamData> discoveryUpstreamDataList = discoveryUpstreamService.findBySelectorId(selectorId);
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setUpstreamDataList(discoveryUpstreamDataList);
        discoverySyncData.setPluginName(pluginName);
        discoverySyncData.setSelectorId(selectorId);
        discoverySyncData.setSelectorName(selectorName);
        discoverySyncData.setNamespaceId(namespaceId);
        return discoverySyncData;
    }

    protected void removeDiscoveryUpstream(final String selectorId, final String url) {
        discoveryUpstreamService.deleteBySelectorIdAndUrl(selectorId, url);
    }

    /**
     * Guard a legacy URI writer in a concrete registration implementation.
     *
     * @param selectorId selector id
     * @param uriList URI payload
     */
    protected void assertLegacyUriWrite(final String selectorId, final List<URIRegisterDTO> uriList) {
        uriList.stream().filter(uri -> Objects.nonNull(uri.getPort()) && StringUtils.isNotBlank(uri.getHost()))
                .forEach(uri -> registrationFencingGuard.assertLegacyUriWrite(selectorId, uri.getHost(), uri.getPort()));
    }

    /**
     * Gets the shared registration fencing guard.
     *
     * @return fencing guard
     */
    protected RegistrationFencingGuard getRegistrationFencingGuard() {
        return registrationFencingGuard;
    }

    /**
     * Execute one registration lifecycle operation in a database transaction.
     * Existing fenced calls join their outer mutation transaction.
     *
     * @param action operation
     * @param <T> result type
     * @return operation result
     */
    protected final <T> T executeRegistrationTransaction(final Supplier<T> action) {
        if (Objects.isNull(transactionManager) || TransactionSynchronizationManager.isActualTransactionActive()) {
            return action.get();
        }
        return new TransactionTemplate(transactionManager).execute(status -> action.get());
    }

    @Override
    public String registerURIWithoutFallback(final String selectorName, final List<URIRegisterDTO> uriList,
                                             final String namespaceId) {
        return doRegisterURI(selectorName, uriList, namespaceId);
    }

    @Override
    public String heartbeatWithoutFallback(final String selectorName, final List<URIRegisterDTO> uriList,
                                           final String namespaceId) {
        return doHeartbeat(selectorName, uriList, namespaceId);
    }

    @Override
    public String drainingWithoutFallback(final String selectorName, final List<URIRegisterDTO> uriList,
                                          final String namespaceId) {
        return doDraining(selectorName, uriList, namespaceId);
    }

    @Override
    public String notReadyWithoutFallback(final String selectorName, final List<URIRegisterDTO> uriList,
                                          final String namespaceId) {
        return doNotReady(selectorName, uriList, namespaceId);
    }

    @Override
    public String readyWithoutFallback(final String selectorName, final List<URIRegisterDTO> uriList,
                                       final String namespaceId) {
        return doReady(selectorName, uriList, namespaceId);
    }

    @Override
    public String offlineWithoutFallback(final String selectorName, final List<URIRegisterDTO> uriList,
                                         final String namespaceId) {
        return offline(selectorName, uriList, namespaceId);
    }

    /**
     * Gets meta data service.
     *
     * @return the meta data service
     */
    public MetaDataService getMetaDataService() {
        return metaDataService;
    }

    /**
     * Gets selector service.
     *
     * @return the selector service
     */
    public SelectorService getSelectorService() {
        return selectorService;
    }

    /**
     * Gets rule service.
     *
     * @return the rule service
     */
    public RuleService getRuleService() {
        return ruleService;
    }

    /**
     * Gets event publisher.
     *
     * @return the event publisher
     */
    public ApplicationEventPublisher getEventPublisher() {
        return eventPublisher;
    }

    /**
     * Do submit.
     *
     * @param selectorId   the selector id
     * @param upstreamList the upstream list
     * @return whether this module handles
     */
    protected boolean doSubmit(final String selectorId, final List<? extends CommonUpstream> upstreamList) {
        List<CommonUpstream> commonUpstreamList = CommonUpstreamUtils.convertCommonUpstreamList(upstreamList);
        if (CollectionUtils.isEmpty(upstreamList)) {
            return true;
        }
        return commonUpstreamList.stream().map(upstream -> upstreamCheckService.checkAndSubmit(selectorId, upstream))
                .toList().stream().findAny().orElse(false);
    }

    /**
     * Sync the changed status back to the exist list, so that the status change
     * can be persisted into the selector handle.
     *
     * <p>The upstream {@code equals} methods only compare the identity fields, so an upstream whose
     * status just changed is still regarded as an existing one and keeps its old status in the exist list.
     *
     * @param existList      the upstream list parsed from the selector handle
     * @param diffStatusList the upstream list whose status has changed
     * @param <T>            the upstream type
     */
    protected <T extends CommonUpstream> void syncUpstreamStatus(final List<T> existList, final List<T> diffStatusList) {
        for (T changed : diffStatusList) {
            existList.stream().filter(exist -> sameUpstreamIdentity(exist, changed))
                    .forEach(exist -> exist.setStatus(changed.isStatus()));
        }
    }

    /**
     * Compare selector members by the canonical endpoint identity. This keeps
     * a historical IPv6 spelling in a selector handle from becoming a second
     * member when a later request uses the canonical spelling.
     *
     * @param left first member
     * @param right second member
     * @return whether both members represent the same registered endpoint
     */
    protected boolean sameUpstreamIdentity(final CommonUpstream left, final CommonUpstream right) {
        if (Objects.isNull(left) || Objects.isNull(right)
                || !Objects.equals(left.getNamespaceId(), right.getNamespaceId())) {
            return false;
        }
        return Objects.equals(canonicalUpstreamEndpoint(left.getUpstreamUrl()), canonicalUpstreamEndpoint(right.getUpstreamUrl()))
                && Objects.equals(canonicalUpstreamProtocol(left.getProtocol()), canonicalUpstreamProtocol(right.getProtocol()));
    }

    private String canonicalUpstreamEndpoint(final String endpoint) {
        try {
            return RegistrationFencingAddress.canonicalEndpoint(endpoint);
        } catch (IllegalArgumentException exception) {
            return endpoint;
        }
    }

    private String canonicalUpstreamProtocol(final String protocol) {
        try {
            return RegistrationFencingAddress.canonicalProtocol(protocol);
        } catch (IllegalArgumentException exception) {
            return protocol;
        }
    }

    /**
     * Build context path default rule dto rule dto.
     *
     * @param selectorId  the selector id
     * @param metaDataDTO the meta data dto
     * @param ruleHandler the rule handler
     * @return the rule dto
     */
    protected RuleDTO buildContextPathDefaultRuleDTO(final String selectorId, final MetaDataRegisterDTO metaDataDTO, final String ruleHandler) {
        String contextPath = metaDataDTO.getContextPath();
        String namespaceId = metaDataDTO.getNamespaceId();
        return buildRuleDTO(selectorId, ruleHandler, contextPath, PathUtils.decoratorPath(contextPath), namespaceId);
    }

    private RuleDTO buildRpcDefaultRuleDTO(final String selectorId, final MetaDataRegisterDTO metaDataDTO, final String ruleHandler) {
        return buildRuleDTO(selectorId, ruleHandler, metaDataDTO.getRuleName(), metaDataDTO.getPath(), metaDataDTO.getNamespaceId());
    }

    private RuleDTO buildRuleDTO(final String selectorId, final String ruleHandler, final String ruleName, final String path, final String namespaceId) {
        RuleDTO ruleDTO = RuleDTO.builder()
                .selectorId(selectorId)
                .name(ruleName)
                .matchMode(MatchModeEnum.AND.getCode())
                .enabled(Boolean.TRUE)
                .logged(Boolean.TRUE)
                .matchRestful(Boolean.FALSE)
                .sort(1)
                .handle(ruleHandler)
                .namespaceId(namespaceId)
                .build();

        String conditionPath = this.rewritePath(path);
        RuleConditionDTO ruleConditionDTO = RuleConditionDTO.builder()
                .paramType(ParamTypeEnum.URI.getName())
                .paramName("/")
                .paramValue(conditionPath)
                .build();
        if (conditionPath.endsWith(AdminConstants.URI_SLASH_SUFFIX)) {
            ruleConditionDTO.setOperator(OperatorEnum.STARTS_WITH.getAlias());
        } else if (conditionPath.endsWith(AdminConstants.URI_SUFFIX)) {
            ruleConditionDTO.setOperator(OperatorEnum.PATH_PATTERN.getAlias());
        } else if (conditionPath.indexOf("*") > 1) {
            ruleConditionDTO.setOperator(OperatorEnum.MATCH.getAlias());
        } else {
            ruleConditionDTO.setOperator(OperatorEnum.EQ.getAlias());
        }
        ruleDTO.setRuleConditions(Collections.singletonList(ruleConditionDTO));
        return ruleDTO;
    }

    /**
     * adjustment such as '/aa/${xxx}/cc' replace to `/aa/`**`/cc` for client simpler annotation.
     * link: https://github.com/apache/shenyu/pull/3819
     *
     * @param path the path
     * @return the replaced path
     */
    private String rewritePath(final String path) {
        if (path.contains(AdminConstants.URI_VARIABLE_SUFFIX)) {
            return path.replaceAll("(/\\{.*?})+", "/**");
        }
        return path;
    }
}
