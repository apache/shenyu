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

package org.apache.shenyu.admin.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.model.query.InstanceQueryCondition;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.service.PageService;
import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shiro.authz.annotation.RequiresPermissions;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * this is instance controller.
 */
@RestApi("/instance")
public class InstanceController implements PagedController<InstanceQueryCondition, InstanceInfoVO> {

    private final InstanceInfoService instanceInfoService;

    private final InstanceCheckService instanceCheckService;

    public InstanceController(final InstanceInfoService instanceInfoService, final InstanceCheckService instanceCheckService) {
        this.instanceInfoService = instanceInfoService;
        this.instanceCheckService = instanceCheckService;
    }

    /**
     * query instance info.
     *
     * @param instanceType instance type.
     * @param instanceIp   instance ip.
     * @param instancePort instance port.
     * @param namespaceId  namespace id.
     * @param currentPage  current page.
     * @param pageSize     page size.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping
    public ShenyuAdminResult queryPlugins(@RequestParam(name = "instanceType", required = false) final String instanceType,
                                          @RequestParam(name = "instanceIp", required = false) final String instanceIp,
                                          @RequestParam(name = "instancePort", required = false) final String instancePort,
                                          @RequestParam(name = "namespaceId") final String namespaceId,
                                          @NotNull @RequestParam(name = "currentPage") final Integer currentPage,
                                          @NotNull @RequestParam(name = "pageSize") final Integer pageSize) {
        CommonPager<InstanceInfoVO> commonPager = instanceInfoService.listByPage(
                new InstanceQuery(
                        new PageParameter(currentPage, pageSize),
                        instanceType,
                        instanceIp,
                        instancePort,
                        namespaceId
                )
        );
        if (!CollectionUtils.isEmpty(commonPager.getDataList())) {
            commonPager.getDataList().forEach(instanceInfoVO -> {
                String instanceKey = instanceCheckService.getInstanceKey(instanceInfoVO);
                InstanceInfoVO instanceHealthBeatInfo = instanceCheckService.getInstanceHealthBeatInfo(instanceKey);
                instanceInfoVO.setLastHeartBeatTime(instanceHealthBeatInfo.getLastHeartBeatTime());
                instanceInfoVO.setInstanceState(instanceHealthBeatInfo.getInstanceState());
                instanceInfoVO.setDateUpdated(instanceHealthBeatInfo.getDateUpdated());
            });
        }
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, commonPager);
    }

    /**
     * detail instance info.
     *
     * @param id instance id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/{id}")
    @RequiresPermissions("system:instance:edit")
    public ShenyuAdminResult detailInstanceInfo(@PathVariable("id") final String id) {
        InstanceInfoVO instanceInfoVO = instanceInfoService.findById(id);
        return ShenyuAdminResult.success(ShenyuResultMessage.DETAIL_SUCCESS, instanceInfoVO);
    }

    /**
     * receive beat.
     *
     * @param instanceBeatInfoDTO instanceBeatInfoDTO.
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("/beat")
    public String beat(@Valid @RequestBody final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        instanceCheckService.handleBeatInfo(instanceBeatInfoDTO);
        return ShenyuResultMessage.SUCCESS;
    }

    /**
     * visual instance info.
     *
     * @param namespaceId namespace id.
     * @return {@linkplain ShenyuAdminResult}
     */
    @GetMapping("/analysis/{namespaceId}")
    public ShenyuAdminResult getInstanceDataVisual(@PathVariable("namespaceId") final String namespaceId) {
        return ShenyuAdminResult.success(ShenyuResultMessage.QUERY_SUCCESS, instanceCheckService.getInstanceDataVisual(namespaceId));
    }


    @Override
    public PageService<InstanceQueryCondition, InstanceInfoVO> pageService() {
        return instanceInfoService;
    }
}
