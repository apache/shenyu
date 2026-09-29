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
import org.apache.shenyu.admin.jpa.repository.ApiRepository;
import org.apache.shenyu.admin.jpa.repository.TagRelationRepository;
import org.apache.shenyu.admin.jpa.repository.TagRepository;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.vo.ApiVO;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Collections;
import java.util.Optional;
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

    private final ApiRepository apiRepository;

    private final TagRepository tagRepository;

    private final TagRelationRepository tagRelationRepository;

    public ApiServiceImpl(final SelectorService selectorService,
                          final RuleService ruleService,
                          final MetaDataService metaDataService,
                          final ApiRepository apiRepository,
                          final TagRepository tagRepository,
                          final TagRelationRepository tagRelationRepository) {
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.metaDataService = metaDataService;
        this.apiRepository = apiRepository;
        this.tagRepository = tagRepository;
        this.tagRelationRepository = tagRelationRepository;
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
        final boolean updated = apiRepository.findById(apiDO.getId())
                .map(persisted -> {
                    copyNonNullFields(apiDO, persisted);
                    return true;
                })
                .orElse(false);
        if (updated) {
            if (Objects.nonNull(apiDTO.getTagIds())) {
                List<String> tagIds = apiDTO.getTagIds();
                tagRelationRepository.deleteByApiId(apiDO.getId());
                if (CollectionUtils.isNotEmpty(tagIds)) {
                    Timestamp currentTime = new Timestamp(System.currentTimeMillis());
                    List<TagRelationDO> tags = tagIds.stream().map(tagId -> TagRelationDO.builder()
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .apiId(apiDO.getId())
                        .tagId(tagId)
                        .dateCreated(currentTime)
                        .dateUpdated(currentTime)
                        .build()).collect(Collectors.toList());
                    tagRelationRepository.saveAll(tags);
                }
            }
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    /**
     * Copy non-null fields from source to target, null fields are skipped like the original selective update.
     *
     * @param source the source built from {@link ApiDTO}
     * @param target the target managed entity
     */
    private void copyNonNullFields(final ApiDO source, final ApiDO target) {
        if (Objects.nonNull(source.getContextPath())) {
            target.setContextPath(source.getContextPath());
        }
        if (Objects.nonNull(source.getApiPath())) {
            target.setApiPath(source.getApiPath());
        }
        if (Objects.nonNull(source.getHttpMethod())) {
            target.setHttpMethod(source.getHttpMethod());
        }
        if (Objects.nonNull(source.getConsume())) {
            target.setConsume(source.getConsume());
        }
        if (Objects.nonNull(source.getProduce())) {
            target.setProduce(source.getProduce());
        }
        if (Objects.nonNull(source.getVersion())) {
            target.setVersion(source.getVersion());
        }
        if (Objects.nonNull(source.getRpcType())) {
            target.setRpcType(source.getRpcType());
        }
        if (Objects.nonNull(source.getState())) {
            target.setState(source.getState());
        }
        if (Objects.nonNull(source.getExt())) {
            target.setExt(source.getExt());
        }
        if (Objects.nonNull(source.getApiOwner())) {
            target.setApiOwner(source.getApiOwner());
        }
        if (Objects.nonNull(source.getApiDesc())) {
            target.setApiDesc(source.getApiDesc());
        }
        if (Objects.nonNull(source.getApiSource())) {
            target.setApiSource(source.getApiSource());
        }
        if (Objects.nonNull(source.getDocument())) {
            target.setDocument(source.getDocument());
            target.setDocumentMd5(source.getDocumentMd5());
        }
    }

    /**
     * create.
     *
     * @param apiDTO apiDTO
     * @return create message
     */
    private String create(final ApiDTO apiDTO) {
        ApiDO apiDO = ApiDO.buildApiDO(apiDTO);
        apiRepository.save(apiDO);
        final int insertRows = 1;
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
                tagRelationRepository.saveAll(tags);
            }
            register(apiDO);
        }
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    private void removeRegister(final ApiDO apiDO) {
        final String path = apiDO.getApiPath();
        RuleQueryCondition condition = new RuleQueryCondition();
        condition.setKeyword(path);
        //clean rule
        final List<RuleVO> rules = ruleService.searchByCondition(condition);
        if (CollectionUtils.isNotEmpty(rules)) {
            ruleService.deleteByIdsAndNamespaceId(rules.stream()
                    .map(RuleVO::getId)
                    .distinct()
                    // todo:[To be refactored with namespace]  Temporarily  hardcode
                    .collect(Collectors.toList()), SYS_DEFAULT_NAMESPACE_ID);
        }
        //clean selector
        List<SelectorDO> selectorDOList = selectorService.findByNameAndPluginNamesAndNamespaceId(apiDO.getContextPath(), PluginEnum.getUpstreamNames(), SYS_DEFAULT_NAMESPACE_ID);
        ArrayList<String> selectorIds = Lists.newArrayList();
        Optional.ofNullable(selectorDOList).orElseGet(ArrayList::new).forEach(selectorDO -> {
            final String selectorId = selectorDO.getId();
            final List<RuleData> data = ruleService.findBySelectorId(selectorId);
            if (CollectionUtils.isEmpty(data)) {
                selectorIds.add(selectorId);
            }
        });
        if (CollectionUtils.isNotEmpty(selectorIds)) {
            // todo:[To be refactored with namespace]  Temporarily  hardcode
            selectorService.deleteByNamespaceId(selectorIds, SYS_DEFAULT_NAMESPACE_ID);
        }
        //clean metadata
        Optional.ofNullable(metaDataService.findByPathAndNamespaceId(path, SYS_DEFAULT_NAMESPACE_ID))
                .ifPresent(metaDataDO -> metaDataService.deleteByIdsAndNamespaceId(Lists.newArrayList(metaDataDO.getId()), SYS_DEFAULT_NAMESPACE_ID));
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
                .enabled(true)
                .build());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> ids) {
        // select api id.
        List<ApiDO> apis = this.apiRepository.findAllById(ids);
        if (CollectionUtils.isEmpty(apis)) {
            return AdminConstants.SYS_API_ID_NOT_EXIST;
        }
        // delete apis.
        final List<String> apiIds = ListUtil.map(apis, ApiDO::getId);
        final int deleteRows = this.apiRepository.deleteByIds(apiIds);
        if (deleteRows > 0) {
            tagRelationRepository.deleteByApiIds(apiIds);
            apis.forEach(this::removeRegister);
        }
        return StringUtils.EMPTY;
    }

    @Override
    public ApiVO findById(final String id) {
        return apiRepository.findById(id).map(item -> {
            List<TagRelationDO> tagRelations = tagRelationRepository.findByApiId(item.getId());
            List<String> tagIds = tagRelations.stream().map(TagRelationDO::getTagId).collect(Collectors.toList());
            List<TagVO> tagVOs = Lists.newArrayList();
            if (CollectionUtils.isNotEmpty(tagIds)) {
                List<TagDO> tagDOS = tagRepository.findAllById(tagIds);
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
    public CommonPager<ApiVO> listByPage(final ApiQuery apiQuery) {
        Page<ApiDO> page = apiRepository.pageByQuery(apiQuery, PageResultUtils.of(apiQuery.getPageParameter()));
        List<ApiDO> apis = page.getContent();
        if (apis.isEmpty()) {
            return PageResultUtils.result(apiQuery.getPageParameter(), Collections::emptyList);
        }
        List<String> apiIds = apis.stream().map(ApiDO::getId).collect(Collectors.toList());
        List<TagRelationDO> relations = tagRelationRepository.findByApiIdIn(apiIds);
        List<String> tagIds = relations.stream().map(TagRelationDO::getTagId).filter(Objects::nonNull).distinct().collect(Collectors.toList());
        Map<String, TagVO> tags = tagIds.isEmpty() ? Collections.emptyMap() : tagRepository.findAllById(tagIds).stream()
                .collect(Collectors.toMap(TagDO::getId, TagVO::buildTagVO));
        Map<String, List<TagRelationDO>> relationsByApi = relations.stream().collect(Collectors.groupingBy(TagRelationDO::getApiId));
        return PageResultUtils.result(apiQuery.getPageParameter(), page, api -> {
            List<TagVO> apiTags = relationsByApi.getOrDefault(api.getId(), Collections.emptyList()).stream()
                    .map(TagRelationDO::getTagId).distinct().map(tags::get).filter(Objects::nonNull).collect(Collectors.toList());
            return ApiVO.buildApiVO(api, apiTags);
        });
    }

    @Override
    public int deleteByApiPathHttpMethodRpcType(final String apiPath, final Integer httpMethod, final String rpcType) {
        List<ApiDO> apiDOs = apiRepository.findByApiPathAndHttpMethodAndRpcType(apiPath, httpMethod, rpcType);
        // delete apis.
        if (CollectionUtils.isNotEmpty(apiDOs)) {
            final List<String> apiIds = ListUtil.map(apiDOs, ApiDO::getId);
            final int deleteRows = this.apiRepository.deleteByIds(apiIds);
            if (deleteRows > 0) {
                tagRelationRepository.deleteByApiIds(apiIds);
                apiDOs.forEach(this::removeRegister);
            }
            return deleteRows;
        }
        return 0;
    }

    @Override
    public String offlineByContextPath(final String contextPath) {
        apiRepository.updateOfflineByContextPath(contextPath);
        return ShenyuResultMessage.SUCCESS;
    }
}
