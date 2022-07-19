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

import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.DocVO;
import org.apache.shenyu.admin.model.vo.MenuDocItem;
import org.apache.shenyu.admin.model.vo.MenuModule;
import org.apache.shenyu.admin.model.vo.MenuProject;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.apache.shenyu.common.constant.AdminConstants;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Api Documet Controller.
 */
@RestController
@RequestMapping("/apidoc")
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
        List<MenuProject> menuProjects = docInfos.stream()
            .map(getMenuAndDocInfo())
            .collect(Collectors.toList());
        DocVO docVO = new DocVO();
        docVO.setMenuProjects(menuProjects);
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

    /**
     * Query the document content according to the document ID.
     * @param id docmentId
     * @return ShenyuAdminResult
     */
    @GetMapping("/getDocItem")
    public ShenyuAdminResult getDocItem(final String id) {
        DocItem docItem = docManager.getDocItem(id);
        return ShenyuAdminResult.success(docItem);
    }

    private Function<DocInfo, MenuProject> getMenuAndDocInfo() {
        return docInfo -> {
            MenuProject menuProject = new MenuProject();
            menuProject.setLabel(docInfo.getTitle());
            List<MenuModule> menuModules = docInfo.getDocModuleList()
                .stream()
                .map(docModule -> {
                    MenuModule menuModule = new MenuModule();
                    menuModule.setLabel(docModule.getModule());
                    List<MenuDocItem> docItems = docModule.getDocItems().stream()
                        .map(docItem -> {
                            MenuDocItem menuDocItem = new MenuDocItem();
                            menuDocItem.setId(docItem.getId());
                            menuDocItem.setLabel(docItem.getSummary());
                            menuDocItem.setName(docItem.getName());
                            return menuDocItem;
                        }).collect(Collectors.toList());
                    menuModule.setChildren(docItems);
                    return menuModule;
                }).collect(Collectors.toList());
            menuProject.setChildren(menuModules);
            return menuProject;
        };
    }

}
