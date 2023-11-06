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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.DocVO;
import org.apache.shenyu.admin.model.vo.MenuDocItemVO;
import org.apache.shenyu.admin.model.vo.MenuModuleVO;
import org.apache.shenyu.admin.model.vo.MenuProjectVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.common.constant.AdminConstants;
import org.springframework.web.bind.annotation.GetMapping;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Api Documet Controller.
 */
@RestApi("/apidoc")
public class ApiDocController {

    private final DocManager docManager;

    private final ShenyuDictService shenyuDictService;

    public ApiDocController(final DocManager docManager,
                            final ShenyuDictService shenyuDictService) {
        this.docManager = docManager;
        this.shenyuDictService = shenyuDictService;
    }

    /**
     * Menu list of documents.
     *
     * @return ShenyuAdminResult
     */
    @GetMapping("/getDocMenus")
    public ShenyuAdminResult getAllDoc() {
        Collection<DocInfo> docInfos = docManager.listAll();
        List<MenuProjectVO> menuProjectList = docInfos.stream()
            .map(getMenuAndDocInfo())
            .collect(Collectors.toList());
        DocVO docVO = new DocVO();
        docVO.setMenuProjects(menuProjectList);
        List<ShenyuDictVO> dictVOList = shenyuDictService.list(AdminConstants.DICT_TYPE_API_DOC_ENV);
        List<DocVO.EnvConfig> envConfigs = dictVOList.stream()
            .filter(ShenyuDictVO::getEnabled)
            .map(dictVO -> {
                DocVO.EnvConfig envConfig = new DocVO.EnvConfig();
                envConfig.setEnvLabel(dictVO.getDictName());
                envConfig.setAddressUrl(dictVO.getDictValue());
                envConfig.setEnvDesc(dictVO.getDesc());
                return envConfig;
            })
            .collect(Collectors.toList());
        docVO.setEnvProps(envConfigs);
        if (CollectionUtils.isNotEmpty(envConfigs)) {
            docVO.setGatewayUrl(envConfigs.get(0).getAddressUrl());
        }
        return ShenyuAdminResult.success(docVO);
    }

    private Function<DocInfo, MenuProjectVO> getMenuAndDocInfo() {
        return docInfo -> {
            MenuProjectVO menuProjectVO = new MenuProjectVO();
            menuProjectVO.setLabel(docInfo.getTitle());
            List<MenuModuleVO> menuProjectList = docInfo.getDocModuleList()
                .stream()
                .map(docModule -> {
                    MenuModuleVO menuModuleVO = new MenuModuleVO();
                    menuModuleVO.setLabel(docModule.getModule());
                    List<MenuDocItemVO> docItems = docModule.getDocItems().stream()
                        .map(docItem -> {
                            MenuDocItemVO menuDocItemVO = new MenuDocItemVO();
                            menuDocItemVO.setLabel(docItem.getSummary());
                            menuDocItemVO.setName(docItem.getName());
                            return menuDocItemVO;
                        }).collect(Collectors.toList());
                    menuModuleVO.setChildren(docItems);
                    return menuModuleVO;
                }).collect(Collectors.toList());
            menuProjectVO.setChildren(menuProjectList);
            return menuProjectVO;
        };
    }

}
