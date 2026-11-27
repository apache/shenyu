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

package org.apache.shenyu.admin.service.impl;

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import java.util.Objects;
import org.apache.shenyu.admin.disruptor.RegisterClientServerDisruptorPublisher;
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.model.query.TagRelationQuery;
import org.apache.shenyu.admin.model.vo.ApiVO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.ContextPathUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.Collections;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ApiService}.
 */
@Service
public class ApiServiceImpl implements ApiService {

    private final SelectorService selectorService;

    private final RuleService ruleService;

    private final MetaDataService metaDataService;

    private final ApiMapper apiMapper;

    private final TagRelationMapper tagRelationMapper;

    private final TagMapper tagMapper;

    public ApiServiceImpl(final SelectorService selectorService,
                          final RuleService ruleService,
                          final MetaDataService metaDataService,
                          final ApiMapper apiMapper,
                          final TagRelationMapper tagRelationMapper,
                          final TagMapper tagMapper) {
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.metaDataService = metaDataService;
        this.apiMapper = apiMapper;
        this.tagRelationMapper = tagRelationMapper;
        this.tagMapper = tagMapper;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String createOrUpdate(final ApiDTO apiDTO) {
        return StringUtils.isBlank(apiDTO.getId()) ? this.create(apiDTO) : this.update(apiDTO);
    }

    /**
     * update.
     *
     * @param apiDTO apiDTO
     * @return update message
     */
    private String update(final ApiDTO apiDTO) {
        ApiDO apiDO = ApiDO.buildApiDO(apiDTO);
        // Source records creation provenance, including historical retired sources.
        // The selective update must retain it even when the client supplies another value.
        apiDO.setApiSource(null);
        final int updateRows = apiMapper.updateByPrimaryKeySelective(apiDO);
        if (updateRows > 0) {
            if (Objects.nonNull(apiDTO.getTagIds())) {
                List<String> tagIds = apiDTO.getTagIds();
                tagRelationMapper.deleteByApiId(apiDO.getId());
                if (CollectionUtils.isNotEmpty(tagIds)) {
                    Timestamp currentTime = new Timestamp(System.currentTimeMillis());
                    List<TagRelationDO> tags = tagIds.stream().map(tagId -> TagRelationDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .apiId(apiDO.getId())
                        .tagId(tagId)
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build()).collect(Collectors.toList());
                    tagRelationMapper.batchInsert(tags);
                }
            }
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    /**
     * create.
     *
     * @param apiDTO apiDTO
     * @return create message
     */
    private String create(final ApiDTO apiDTO) {
        ApiDO apiDO = ApiDO.buildApiDO(apiDTO);
        final int insertRows = apiMapper.insertSelective(apiDO);
        if (insertRows > 0) {
            //create tag relation
            if (CollectionUtils.isNotEmpty(apiDTO.getTagIds())) {
                List<String> tagIds = apiDTO.getTagIds();
                Timestamp currentTime = new Timestamp(System.currentTimeMillis());
                List<TagRelationDO> tags = tagIds.stream().map(tagId -> TagRelationDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .apiId(apiDO.getId())
                        .tagId(tagId)
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build()).collect(Collectors.toList());
                tagRelationMapper.batchInsert(tags);
            }
            register(apiDO);
        }
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    private void removeRegister(final ApiDO apiDO,
                                final BiConsumer<List<String>, String> ruleRemover,
                                final BiConsumer<List<String>, String> selectorRemover) {
        removeRegisteredRoute(apiDO, ruleRemover, selectorRemover);
        Optional.ofNullable(metaDataService.findByPathAndNamespaceId(apiDO.getApiPath(), SYS_DEFAULT_NAMESPACE_ID))
                .ifPresent(metaDataDO -> metaDataService.deleteByIdsAndNamespaceId(Lists.newArrayList(metaDataDO.getId()), SYS_DEFAULT_NAMESPACE_ID));
    }

    private void removeRegisteredRoute(final ApiDO apiDO,
                                       final BiConsumer<List<String>, String> ruleRemover,
                                       final BiConsumer<List<String>, String> selectorRemover) {
        final String contextPath = apiDO.getContextPath();
        final String rpcType = apiDO.getRpcType();
        // AI does not use this registration path; the adapter would otherwise fall back to Divide.
        final boolean supportsRegistration = RpcTypeEnum.acquireSupports().stream()
                .anyMatch(type -> type != RpcTypeEnum.AI && type.getName().equals(rpcType));
        if (StringUtils.isBlank(contextPath) || !supportsRegistration) {
            return;
        }
        final SelectorDO selector = selectorService.findByNameAndPluginNameAndNamespaceId(
                ContextPathUtils.buildContextPath(contextPath, null), PluginNameAdapter.rpcTypeAdapter(rpcType), SYS_DEFAULT_NAMESPACE_ID);
        if (Objects.isNull(selector)) {
            return;
        }
        final String selectorId = selector.getId();
        final String path = apiDO.getApiPath();
        final RuleDO rule = ruleService.findBySelectorIdAndName(selectorId, path);
        if (Objects.isNull(rule) || !Objects.equals(path, rule.getRuleName())) {
            return;
        }
        ruleRemover.accept(List.of(rule.getId()), SYS_DEFAULT_NAMESPACE_ID);
        if (CollectionUtils.isEmpty(ruleService.findBySelectorId(selectorId))) {
            selectorRemover.accept(List.of(selectorId), SYS_DEFAULT_NAMESPACE_ID);
        }
    }

    private void register(final ApiDO apiDO) {
        //register selector/rule/metadata if necessary
        final ApiDocRegisterDTO.ApiExt ext = GsonUtils.getInstance().fromJson(apiDO.getExt(), ApiDocRegisterDTO.ApiExt.class);
        if (Objects.isNull(ext) || StringUtils.isBlank(apiDO.getContextPath())) {
            return;
        }
        RegisterClientServerDisruptorPublisher publisher = RegisterClientServerDisruptorPublisher.getInstance();
        final String contextPath = apiDO.getContextPath();
        final String path = apiDO.getApiPath();
        final String appName = contextPath.substring(1);
        final String host = ext.getHost();
        final Integer port = ext.getPort();
        publisher.publish(MetaDataRegisterDTO.builder()
                .addPrefixed(ext.isAddPrefixed())
                .appName(appName)
                .serviceName(ext.getServiceName())
                .methodName(ext.getMethodName())
                .contextPath(contextPath)
                .host(host)
                .port(port)
                .path(path)
                .ruleName(path)
                .pathDesc(apiDO.getApiDesc())
                .parameterTypes(ext.getParameterTypes())
                .rpcExt(ext.getRpcExt())
                .rpcType(apiDO.getRpcType())
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .enabled(true)
                .build());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> ids) {
        // select api id.
        List<ApiDO> apis = this.apiMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(apis)) {
            return AdminConstants.SYS_API_ID_NOT_EXIST;
        }
        // delete apis.
        final List<String> apiIds = ListUtil.map(apis, ApiDO::getId);
        final int deleteRows = this.apiMapper.deleteByIds(apiIds);
        if (deleteRows > 0) {
            tagRelationMapper.deleteByApiIds(apiIds);
            // a user deletion goes through the plugin data permission checks of the selector and rule services
            apis.forEach(api -> removeRegister(api, ruleService::deleteByIdsAndNamespaceId, selectorService::deleteByNamespaceId));
        }
        return StringUtils.EMPTY;
    }

    @Override
    public ApiVO findById(final String id) {
        return Optional.ofNullable(apiMapper.selectByPrimaryKey(id)).map(item -> {
            List<TagRelationDO> tagRelations = tagRelationMapper.selectByQuery(TagRelationQuery.builder().apiId(item.getId()).build());
            List<String> tagIds = tagRelations.stream().map(TagRelationDO::getTagId).collect(Collectors.toList());
            List<TagVO> tagVOs = Lists.newArrayList();
            if (CollectionUtils.isNotEmpty(tagIds)) {
                List<TagDO> tagDOS = tagMapper.selectByIds(tagIds);
                tagVOs = tagDOS.stream().map(TagVO::buildTagVO).collect(Collectors.toList());
            }
            ApiVO apiVO = ApiVO.buildApiVO(item, tagVOs);
            if (StringUtils.isNotBlank(apiVO.getDocument())) {
                DocItem docItem = JsonUtils.jsonToObject(apiVO.getDocument(), DocItem.class);
                if (Objects.nonNull(docItem)) {
                    apiVO.setRequestHeaders(docItem.getRequestHeaders());
                    apiVO.setRequestParameters(docItem.getRequestParameters());
                    apiVO.setResponseParameters(docItem.getResponseParameters());
                    apiVO.setBizCustomCodeList(docItem.getBizCodeList());
                }
            }
            return apiVO;

        }).orElse(null);
    }

    @Override
    @Pageable
    public CommonPager<ApiVO> listByPage(final ApiQuery apiQuery) {
        List<ApiDO> apis = apiMapper.selectByQuery(apiQuery);
        if (apis.isEmpty()) {
            return PageResultUtils.result(apiQuery.getPageParameter(), Collections::emptyList);
        }
        List<String> apiIds = apis.stream().map(ApiDO::getId).collect(Collectors.toList());
        List<TagRelationDO> relations = tagRelationMapper.selectByApiIds(apiIds);
        List<String> tagIds = relations.stream().map(TagRelationDO::getTagId).filter(Objects::nonNull).distinct().collect(Collectors.toList());
        Map<String, TagVO> tags = tagIds.isEmpty() ? Collections.emptyMap() : tagMapper.selectByIds(tagIds).stream()
                .collect(Collectors.toMap(TagDO::getId, TagVO::buildTagVO));
        Map<String, List<TagRelationDO>> relationsByApi = relations.stream().collect(Collectors.groupingBy(TagRelationDO::getApiId));
        return PageResultUtils.result(apiQuery.getPageParameter(), () -> apis.stream().map(api -> {
            List<TagVO> apiTags = relationsByApi.getOrDefault(api.getId(), Collections.emptyList()).stream()
                    .map(TagRelationDO::getTagId).distinct().map(tags::get).filter(Objects::nonNull).collect(Collectors.toList());
            return ApiVO.buildApiVO(api, apiTags);
        }).collect(Collectors.toList()));
    }

    @Override
    public int deleteByApiPathHttpMethodRpcType(final String apiPath, final Integer httpMethod, final String rpcType) {
        List<ApiDO> apiDOs = apiMapper.selectByApiPathHttpMethodRpcType(apiPath, httpMethod, rpcType);
        // delete apis.
        if (CollectionUtils.isNotEmpty(apiDOs)) {
            final List<String> apiIds = ListUtil.map(apiDOs, ApiDO::getId);
            final int deleteRows = this.apiMapper.deleteByIds(apiIds);
            if (deleteRows > 0) {
                tagRelationMapper.deleteByApiIds(apiIds);
                // registration has no login user, so it uses the unchecked registration deletes
                apiDOs.forEach(api -> removeRegister(api, ruleService::deleteForRegistration, selectorService::deleteForRegistration));
            }
            return deleteRows;
        }
        return 0;
    }

    @Override
    public String offlineByContextPath(final String contextPath) {
        apiMapper.updateOfflineByContextPath(contextPath);
        return ShenyuResultMessage.SUCCESS;
    }
}
