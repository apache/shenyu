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
import javax.annotation.Resource;
import org.apache.shenyu.admin.config.properties.ApiDocProperties;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.DocVO;
import org.apache.shenyu.admin.model.vo.MenuDocItem;
import org.apache.shenyu.admin.model.vo.MenuModule;
import org.apache.shenyu.admin.model.vo.MenuProject;
import org.apache.shenyu.admin.service.manager.DocManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * ApiDoc Controller.
 */
@RestController
@RequestMapping("/apidoc")
public class ApiDocController {

    @Autowired
    private DocManager docManager;

    @Resource
    private ApiDocProperties apiDocProperties;

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
        docVO.setGatewayUrl(apiDocProperties.getGatewayUrl());
        docVO.setMenuProjects(menuProjects);
        docVO.setEnvProps(apiDocProperties.getEnvProps());
        docVO.setCookie("Fill in the real cookie value.(signature authentication and login free API ignore this item)");
        docVO.setAppKey("");
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
