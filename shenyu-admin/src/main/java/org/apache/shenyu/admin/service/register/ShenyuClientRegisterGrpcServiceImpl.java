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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.converter.GrpcSelectorHandleConverter;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

/**
 * grpc service register.
 */
@Service
public class ShenyuClientRegisterGrpcServiceImpl extends AbstractShenyuClientRegisterServiceImpl {

    @Resource
    private GrpcSelectorHandleConverter grpcSelectorHandleConverter;

    @Override
    public String rpcType() {
        return RpcTypeEnum.GRPC.getName();
    }

    @Override
    protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
        return "";
    }

    @Override
    protected String ruleHandler() {
        return "";
    }

    @Override
    protected void registerMetadata(final MetaDataRegisterDTO metaDataDTO) {
        MetaDataService metaDataService = getMetaDataService();
        MetaDataDO exist = metaDataService.findByPath(metaDataDTO.getPath());
        metaDataService.saveOrUpdateMetaData(exist, metaDataDTO);
    }

    /**
     * Build handle string.
     *
     * @param uriList    the uri list
     * @param selectorDO the selector do
     * @return the string
     */
    @Override
    protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
        List<GrpcUpstream> addList = buildGrpcUpstreamList(uriList);
        List<GrpcUpstream> canAddList = new CopyOnWriteArrayList<>();
        boolean isEventDeleted = uriList.size() == 1 && EventType.DELETED.equals(uriList.get(0).getEventType());
        if (isEventDeleted) {
            addList.get(0).setStatus(false);
        }
        List<GrpcUpstream> existList = GsonUtils.getInstance().fromCurrentList(selectorDO.getHandle(), GrpcUpstream.class);
        if (CollectionUtils.isEmpty(existList)) {
            canAddList = addList;
        } else {
            List<GrpcUpstream> diffList = addList.stream().filter(upstream -> !existList.contains(upstream)).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(diffList)) {
                canAddList.addAll(diffList);
                existList.addAll(diffList);
            }
            List<GrpcUpstream> diffStatusList = addList.stream().filter(upstream -> !upstream.isStatus()
                    || existList.stream().anyMatch(e -> e.equals(upstream) && e.isStatus() != upstream.isStatus())).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(diffStatusList)) {
                canAddList.addAll(diffStatusList);
            }
        }

        if (doSubmit(selectorDO.getId(), canAddList)) {
            return null;
        }

        List<GrpcUpstream> handleList;
        if (CollectionUtils.isEmpty(existList)) {
            handleList = addList;
        } else {
            List<GrpcUpstream> aliveList;
            if (isEventDeleted) {
                aliveList = existList.stream().filter(e -> e.isStatus() && !e.equals(addList.get(0))).collect(Collectors.toList());
            } else {
                aliveList = addList;
            }
            handleList = grpcSelectorHandleConverter.updateStatusAndFilter(existList, aliveList);
        }
        return GsonUtils.getInstance().toJson(handleList);
    }

    private List<GrpcUpstream> buildGrpcUpstreamList(final List<URIRegisterDTO> uriList) {
        return uriList.stream()
                .map(dto -> CommonUpstreamUtils.buildDefaultGrpcUpstream(dto.getHost(), dto.getPort()))
                .collect(Collectors.toCollection(CopyOnWriteArrayList::new));
    }
}
