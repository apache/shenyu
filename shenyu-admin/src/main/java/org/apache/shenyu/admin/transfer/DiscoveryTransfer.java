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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryRelDTO;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.vo.DiscoveryHandlerVO;
import org.apache.shenyu.admin.model.vo.DiscoveryRelVO;
import org.apache.shenyu.admin.model.vo.DiscoveryUpstreamVO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.admin.utils.CommonUpstreamUtils;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Optional;
import java.util.Properties;

/**
 * DiscoveryTransfer.
 */
public enum DiscoveryTransfer {
    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * mapToDo.
     *
     * @param discoveryUpstreamData discoveryUpstreamData
     * @return DiscoveryUpstreamDO
     */
    public DiscoveryUpstreamDO mapToDo(DiscoveryUpstreamData discoveryUpstreamData) {
        return Optional.ofNullable(discoveryUpstreamData).map(data -> DiscoveryUpstreamDO.builder()
            .discoveryHandlerId(data.getDiscoveryHandlerId())
            .id(data.getId())
            .namespaceId(data.getNamespaceId())
            .protocol(data.getProtocol())
            .status(data.getStatus())
            .weight(data.getWeight())
            .props(data.getProps())
            .url(data.getUrl())
            .dateUpdated(data.getDateUpdated())
            .dateCreated(data.getDateCreated()).build()).orElse(null);
    }
    
    /**
     * mapToCommonUpstream.
     *
     * @param discoveryUpstreamData discoveryUpstreamData
     * @return CommonUpstream
     */
    public CommonUpstream mapToCommonUpstream(DiscoveryUpstreamData discoveryUpstreamData) {
        return Optional.ofNullable(discoveryUpstreamData).map(data -> {
            String url = data.getUrl();
            return new CommonUpstream(data.getProtocol(), url.split(":")[0], url, false, data.getDateCreated().getTime());
        }).orElse(null);
    }

    /**
     * mapToVo.
     *
     * @param discoveryUpstreamDO discoveryUpstreamDO
     * @return DiscoveryUpstreamVO
     */
    public DiscoveryUpstreamVO mapToVo(DiscoveryUpstreamDO discoveryUpstreamDO) {
        return Optional.ofNullable(discoveryUpstreamDO).map(data -> {
            DiscoveryUpstreamVO vo = new DiscoveryUpstreamVO();
            vo.setId(data.getId());
            vo.setDiscoveryHandlerId(data.getDiscoveryHandlerId());
            vo.setProtocol(data.getProtocol());
            vo.setUrl(data.getUrl());
            vo.setStatus(data.getStatus());
            vo.setWeight(data.getWeight());
            vo.setProps(data.getProps());
            vo.setStartupTime(String.valueOf(data.getDateCreated().getTime()));
            return vo;
        }).orElse(null);
    }


    public DiscoveryRelVO mapToVo(DiscoveryRelDO discoveryRelDO) {
        return Optional.ofNullable(discoveryRelDO).map(data -> {
            DiscoveryRelVO discoveryRelVO = new DiscoveryRelVO();
            discoveryRelVO.setId(data.getId());
            discoveryRelVO.setPluginName(data.getPluginName());
            discoveryRelVO.setDiscoveryHandlerId(data.getDiscoveryHandlerId());
            discoveryRelVO.setSelectorId(data.getSelectorId());
            discoveryRelVO.setProxySelectorId(data.getProxySelectorId());
            return discoveryRelVO;
        }).orElse(null);
    }


    public DiscoveryRelDO mapToDO(DiscoveryRelDTO discoveryRelDTO) {
        return Optional.ofNullable(discoveryRelDTO).map(data -> {
            DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
            discoveryRelDO.setId(data.getId());
            discoveryRelDO.setPluginName(data.getPluginName());
            discoveryRelDO.setDiscoveryHandlerId(data.getDiscoveryHandlerId());
            discoveryRelDO.setSelectorId(data.getSelectorId());
            discoveryRelDO.setProxySelectorId(data.getProxySelectorId());
            return discoveryRelDO;
        }).orElse(null);
    }

    public DiscoveryVO mapToVo(DiscoveryDO discoveryDO) {
        return Optional.ofNullable(discoveryDO).map(data -> {
            DiscoveryVO discoveryVO = new DiscoveryVO();
            discoveryVO.setId(data.getId());
            discoveryVO.setName(data.getName());
            discoveryVO.setNamespaceId(data.getNamespaceId());
            discoveryVO.setType(data.getType());
            discoveryVO.setLevel(data.getLevel());
            discoveryVO.setServerList(data.getServerList());
            discoveryVO.setPluginName(data.getPluginName());
            discoveryVO.setProps(data.getProps());
            return discoveryVO;
        }).orElse(null);
    }

    public DiscoveryDTO mapToDTO(DiscoveryDO discoveryDO) {
        return Optional.ofNullable(discoveryDO).map(data -> {
            DiscoveryDTO discoveryDTO = new DiscoveryDTO();
            discoveryDTO.setId(data.getId());
            discoveryDTO.setName(data.getName());
            discoveryDTO.setType(data.getType());
            discoveryDTO.setLevel(data.getLevel());
            discoveryDTO.setServerList(data.getServerList());
            discoveryDTO.setPluginName(data.getPluginName());
            discoveryDTO.setProps(data.getProps());
            return discoveryDTO;
        }).orElse(null);
    }

    public DiscoveryHandlerVO mapToVo(DiscoveryHandlerDO discoveryDO) {
        return Optional.ofNullable(discoveryDO).map(data -> {
            DiscoveryHandlerVO vo = new DiscoveryHandlerVO();
            vo.setId(data.getId());
            vo.setDiscoveryId(data.getDiscoveryId());
            vo.setHandler(data.getHandler());
            vo.setListenerNode(data.getListenerNode());
            vo.setProps(data.getProps());
            return vo;
        }).orElse(null);
    }

    /**
     * mapToData.
     *
     * @param discoveryUpstreamDO discoveryUpstreamDO
     * @return DiscoveryUpstreamData
     */
    public DiscoveryUpstreamData mapToData(DiscoveryUpstreamDO discoveryUpstreamDO) {
        return Optional.ofNullable(discoveryUpstreamDO).map(data -> {
            DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
            discoveryUpstreamData.setId(data.getId());
            discoveryUpstreamData.setProtocol(data.getProtocol());
            discoveryUpstreamData.setUrl(data.getUrl());
            discoveryUpstreamData.setStatus(data.getStatus());
            discoveryUpstreamData.setDiscoveryHandlerId(data.getDiscoveryHandlerId());
            discoveryUpstreamData.setWeight(data.getWeight());
            discoveryUpstreamData.setProps(data.getProps());
            discoveryUpstreamData.setDateUpdated(data.getDateUpdated());
            discoveryUpstreamData.setDateCreated(data.getDateCreated());
            return discoveryUpstreamData;
        }).orElse(null);
    }

    /**
     * mapToData.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return DiscoveryUpstreamData
     */
    public DiscoveryUpstreamData mapToData(DiscoveryUpstreamDTO discoveryUpstreamDTO) {
        return Optional.ofNullable(discoveryUpstreamDTO).map(data -> {
            DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
            discoveryUpstreamData.setId(data.getId());
            discoveryUpstreamData.setProtocol(data.getProtocol());
            discoveryUpstreamData.setUrl(data.getUrl());
            discoveryUpstreamData.setStatus(data.getStatus());
            discoveryUpstreamData.setDiscoveryHandlerId(data.getDiscoveryHandlerId());
            discoveryUpstreamData.setWeight(data.getWeight());
            discoveryUpstreamData.setProps(data.getProps());
            discoveryUpstreamData.setNamespaceId(data.getNamespaceId());
            discoveryUpstreamData.setDateCreated(data.getDateCreated());
            discoveryUpstreamData.setDateUpdated(data.getDateUpdated());
            return discoveryUpstreamData;
        }).orElse(null);
    }

    /**
     * mapToData.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return ProxySelectorData
     */
    public ProxySelectorData mapToData(ProxySelectorDTO proxySelectorDTO) {
        return Optional.ofNullable(proxySelectorDTO).map(data -> {
            ProxySelectorData proxySelectorData = new ProxySelectorData();
            proxySelectorData.setId(data.getId());
            proxySelectorData.setName(data.getName());
            proxySelectorData.setPluginName(data.getPluginName());
            proxySelectorData.setType(data.getType());
            proxySelectorData.setForwardPort(data.getForwardPort());
            proxySelectorData.setNamespaceId(data.getNamespaceId());
            String props = data.getProps();
            Properties properties = GsonUtils.getInstance().fromJson(props, Properties.class);
            proxySelectorData.setProps(properties);
            return proxySelectorData;
        }).orElse(null);
    }

    /**
     * mapToData.
     *
     * @param proxySelectorDO proxySelectorDO
     * @return ProxySelectorData
     */
    public ProxySelectorData mapToData(ProxySelectorDO proxySelectorDO) {
        return Optional.ofNullable(proxySelectorDO).map(data -> {
            ProxySelectorData proxySelectorData = new ProxySelectorData();
            proxySelectorData.setId(data.getId());
            proxySelectorData.setName(data.getName());
            proxySelectorData.setPluginName(data.getPluginName());
            proxySelectorData.setType(data.getType());
            proxySelectorData.setForwardPort(data.getForwardPort());
            String props = data.getProps();
            Properties properties = GsonUtils.getInstance().fromJson(props, Properties.class);
            proxySelectorData.setProps(properties);
            return proxySelectorData;
        }).orElse(null);
    }

    /**
     * mapToDTO.
     *
     * @param proxySelectorDO proxySelectorDO
     * @return ProxySelectorDTO
     */
    public ProxySelectorDTO mapToDTO(ProxySelectorDO proxySelectorDO) {
        return Optional.ofNullable(proxySelectorDO).map(data -> {
            ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
            proxySelectorDTO.setId(data.getId());
            proxySelectorDTO.setName(data.getName());
            proxySelectorDTO.setType(data.getType());
            proxySelectorDTO.setProps(data.getProps());
            proxySelectorDTO.setForwardPort(data.getForwardPort());
            proxySelectorDTO.setPluginName(data.getPluginName());
            proxySelectorDTO.setNamespaceId(data.getNamespaceId());
            return proxySelectorDTO;
        }).orElse(null);
    }

    /**
     * mapToDTO.
     *
     * @param discoveryHandlerDO discoveryHandlerDO
     * @return DiscoveryHandlerDTO
     */
    public DiscoveryHandlerDTO mapToDTO(DiscoveryHandlerDO discoveryHandlerDO) {
        return Optional.ofNullable(discoveryHandlerDO).map(data -> {
            DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
            discoveryHandlerDTO.setDiscoveryId(data.getDiscoveryId());
            discoveryHandlerDTO.setHandler(data.getHandler());
            discoveryHandlerDTO.setProps(data.getProps());
            discoveryHandlerDTO.setListenerNode(data.getListenerNode());
            discoveryHandlerDTO.setId(data.getId());
            return discoveryHandlerDTO;
        }).orElse(null);
    }

    /**
     * mapToDO.
     *
     * @param discoveryHandlerDTO discoveryHandlerDTO
     * @return DiscoveryHandlerDTO
     */
    public DiscoveryHandlerDO mapToDO(DiscoveryHandlerDTO discoveryHandlerDTO) {
        return Optional.ofNullable(discoveryHandlerDTO).map(data -> {
            DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
            discoveryHandlerDO.setDiscoveryId(data.getDiscoveryId());
            discoveryHandlerDO.setHandler(data.getHandler());
            discoveryHandlerDO.setProps(data.getProps());
            discoveryHandlerDO.setListenerNode(data.getListenerNode());
            discoveryHandlerDO.setId(data.getId());
            return discoveryHandlerDO;
        }).orElse(null);
    }

    /**
     * mapToDTO.
     *
     * @param discoveryUpstreamDO discoveryUpstreamDO
     * @return DiscoveryUpstreamDTO
     */
    public DiscoveryUpstreamDTO mapToDTO(DiscoveryUpstreamDO discoveryUpstreamDO) {
        return Optional.ofNullable(discoveryUpstreamDO).map(data -> {
            DiscoveryUpstreamDTO discoveryUpstreamDTO = new DiscoveryUpstreamDTO();
            discoveryUpstreamDTO.setProps(data.getProps());
            discoveryUpstreamDTO.setStatus(data.getStatus());
            discoveryUpstreamDTO.setUrl(data.getUrl());
            discoveryUpstreamDTO.setDiscoveryHandlerId(data.getDiscoveryHandlerId());
            discoveryUpstreamDTO.setProtocol(data.getProtocol());
            discoveryUpstreamDTO.setId(data.getId());
            discoveryUpstreamDTO.setWeight(data.getWeight());
            discoveryUpstreamDTO.setDateCreated(data.getDateCreated());
            discoveryUpstreamDTO.setDateUpdated(data.getDateUpdated());
            return discoveryUpstreamDTO;
        }).orElse(null);
    }
    
    /**
     * mapToDiscoveryUpstreamData.
     * @param commonUpstream commonUpstream
     * @return DiscoveryUpstreamData
     */
    public DiscoveryUpstreamData mapToDiscoveryUpstreamData(CommonUpstream commonUpstream) {
        return mapToData(CommonUpstreamUtils.buildDefaultDiscoveryUpstreamDTO(commonUpstream.getUpstreamUrl().split(":")[0], Integer.valueOf(commonUpstream.getUpstreamUrl().split(":")[1]), commonUpstream.getProtocol(),commonUpstream.getNamespaceId()));
    }
}
