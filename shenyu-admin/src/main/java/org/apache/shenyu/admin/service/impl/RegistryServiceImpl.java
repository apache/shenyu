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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.RegistryMapper;
import org.apache.shenyu.admin.model.dto.RegistryDTO;
import org.apache.shenyu.admin.model.entity.RegistryDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RegistryQuery;
import org.apache.shenyu.admin.model.vo.RegistryVO;
import org.apache.shenyu.admin.service.RegistryService;
import org.apache.shenyu.admin.transfer.RegistryTransfer;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
public class RegistryServiceImpl implements RegistryService {

    private final RegistryMapper registryMapper;

    public RegistryServiceImpl(final RegistryMapper registryMapper) {
        this.registryMapper = registryMapper;
    }

    @Override
    public RegistryVO createOrUpdate(final RegistryDTO registryDTO) {
        return StringUtils.isBlank(registryDTO.getId())
                ? this.create(registryDTO) : this.update(registryDTO);
    }

    @Override
    public CommonPager<RegistryVO> listByPage(final RegistryQuery registryQuery) {
        return PageResultUtils.result(registryQuery.getPageParameter(), () -> registryMapper.countByQuery(registryQuery), () -> registryMapper.selectByQuery(registryQuery)
                .stream()
                .map(RegistryTransfer.INSTANCE::mapToVo)
                .collect(Collectors.toList()));
    }

    @Override
    public String delete(final List<String> ids) {
        registryMapper.deleteByIds(ids);
        return ShenyuResultMessage.DELETE_SUCCESS;
    }

    @Override
    public RegistryVO findById(final String id) {
        return RegistryTransfer.INSTANCE.mapToVo(registryMapper.selectById(id));
    }

    @Override
    public RegistryVO findByRegistryId(final String registryId) {
        return RegistryTransfer.INSTANCE.mapToVo(registryMapper.selectByRegistryId(registryId));
    }

    @Override
    public List<RegistryVO> listAll() {
        List<RegistryDO> registryDOS = registryMapper.selectAll();
        return registryDOS.stream().map(RegistryTransfer.INSTANCE::mapToVo).collect(Collectors.toList());
    }


    private RegistryVO create(final RegistryDTO registryDTO) {
        RegistryDO existRegistryDO = registryMapper.selectByRegistryId(registryDTO.getRegistryId());
        if (Objects.nonNull(existRegistryDO)) {
            throw new ShenyuAdminException("registry_id is already exist");
        }

        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        RegistryDO registryDO = RegistryDO.builder()
                .id(id)
                .registryId(registryDTO.getRegistryId())
                .protocol(registryDTO.getProtocol())
                .address(registryDTO.getAddress())
                .namespace(registryDTO.getNamespace())
                .username(registryDTO.getUsername())
                .password(registryDTO.getPassword())
                .registryGroup(registryDTO.getGroup())
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build();
        registryMapper.insert(registryDO);

        return RegistryTransfer.INSTANCE.mapToVo(registryDO);
    }

    private RegistryVO update(final RegistryDTO registryDTO) {
        if (Objects.isNull(registryDTO) || Objects.isNull(registryDTO.getId())) {
            throw new ShenyuAdminException("registry is not exist");
        }
        RegistryDO existRegistryDO = registryMapper.selectByRegistryId(registryDTO.getRegistryId());
        if (Objects.nonNull(existRegistryDO) && !existRegistryDO.getId().equals(registryDTO.getId())) {
            throw new ShenyuAdminException("registry_id is already exist");
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        RegistryDO registryDO = RegistryDO.builder()
                .id(registryDTO.getId())
                .registryId(registryDTO.getRegistryId())
                .protocol(registryDTO.getProtocol())
                .address(registryDTO.getAddress())
                .namespace(registryDTO.getNamespace())
                .username(registryDTO.getUsername())
                .password(registryDTO.getPassword())
                .registryGroup(registryDTO.getGroup())
                .dateUpdated(currentTime)
                .build();
        return registryMapper.updateSelective(registryDO) > 0
                ? RegistryTransfer.INSTANCE.mapToVo(registryDO) : null;
    }
}
