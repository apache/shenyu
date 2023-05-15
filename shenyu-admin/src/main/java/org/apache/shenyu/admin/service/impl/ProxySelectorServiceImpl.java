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

import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.admin.model.vo.ProxySelectorVO;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ProxySelectorService}.
 */
@Service
public class ProxySelectorServiceImpl implements ProxySelectorService {

    private final ProxySelectorMapper proxySelectorMapper;

    public ProxySelectorServiceImpl(final ProxySelectorMapper proxySelectorMapper) {

        this.proxySelectorMapper = proxySelectorMapper;
    }

    /**
     * listByPage.
     *
     * @param query query
     * @return page
     */
    @Override
    @Pageable
    public CommonPager<ProxySelectorVO> listByPage(final ProxySelectorQuery query) {

        return PageResultUtils.result(query.getPageParameter(), () -> proxySelectorMapper.selectByQuery(query)
                .stream()
                .map(ProxySelectorVO::buildProxySelectorVO)
                .collect(Collectors.toList()));
    }

    /**
     * createOrUpdate.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return the string
     */
    @Override
    public String createOrUpdate(final ProxySelectorDTO proxySelectorDTO) {

        return StringUtils.hasLength(proxySelectorDTO.getId()) ? update(proxySelectorDTO) : create(proxySelectorDTO);
    }

    /**
     * delete.
     *
     * @param ids id list
     * @return the string
     */
    @Override
    public String delete(final List<String> ids) {

        proxySelectorMapper.deleteByIds(ids);
        return ShenyuResultMessage.DELETE_SUCCESS;
    }

    /**
     * create.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return the string
     */
    private String create(final ProxySelectorDTO proxySelectorDTO) {

        Assert.isNull(proxySelectorMapper.nameExisted(proxySelectorDTO.getName()),
                AdminConstants.PROXY_SELECTOR_NAME_IS_EXIST);
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorDTO);
        proxySelectorMapper.insert(proxySelectorDO);
        return ShenyuResultMessage.CREATE_SUCCESS;

    }

    /**
     * update.
     *
     * @param proxySelectorDTO proxySelectorDTO
     * @return the string
     */
    private String update(final ProxySelectorDTO proxySelectorDTO) {

        Assert.isNull(proxySelectorMapper.nameExisted(proxySelectorDTO.getName()),
                AdminConstants.PROXY_SELECTOR_NAME_IS_EXIST);
        ProxySelectorDO proxySelectorDO = ProxySelectorDO.buildProxySelectorDO(proxySelectorDTO);
        proxySelectorMapper.update(proxySelectorDO);
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }
}
