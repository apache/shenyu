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

package org.apache.shenyu.examples.sofa.service.impl;

import org.apache.shenyu.client.sofa.common.annotation.ShenyuSofaClient;
import org.apache.shenyu.examples.sofa.api.entity.SofaComplexTypeBean;
import org.apache.shenyu.examples.sofa.api.entity.SofaSimpleTypeBean;
import org.apache.shenyu.examples.sofa.api.service.SofaMultiParamService;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Sofa multi parameter service.
 */
@Service("sofaMultiParamService")
public class SofaMultiParamServiceImpl implements SofaMultiParamService {

    @Override
    @ShenyuSofaClient("/findByIdsAndName")
    public SofaSimpleTypeBean findByIdsAndName(final List<Integer> ids, final String name) {
        return new SofaSimpleTypeBean(ids.toString(), "hello world shenyu sofa param findByIdsAndName ：" + name);
    }

    @Override
    @ShenyuSofaClient("/findByArrayIdsAndName")
    public SofaSimpleTypeBean findByArrayIdsAndName(final Integer[] ids, final String name) {
        return new SofaSimpleTypeBean(Arrays.toString(ids), "hello world shenyu sofa param findByArrayIdsAndName ：" + name);
    }

    @Override
    @ShenyuSofaClient("/findByStringArray")
    public SofaSimpleTypeBean findByStringArray(final String[] ids) {
        return new SofaSimpleTypeBean(Arrays.toString(ids), "hello world shenyu sofa param findByStringArray");
    }

    @Override
    @ShenyuSofaClient("/findByListId")
    public SofaSimpleTypeBean findByListId(final List<String> ids) {
        return new SofaSimpleTypeBean(ids.toString(), "hello world shenyu sofa param findByListId");
    }

    @Override
    @ShenyuSofaClient("/batchSave")
    public SofaSimpleTypeBean batchSave(final List<SofaSimpleTypeBean> sofaTestList) {
        final String id = sofaTestList.stream().map(SofaSimpleTypeBean::getId).collect(Collectors.joining("-"));
        final String name = "hello world shenyu sofa param batchSave :"
                + sofaTestList.stream()
                .map(SofaSimpleTypeBean::getName)
                .collect(Collectors.joining("-"));
        return new SofaSimpleTypeBean(id, name);
    }

    @Override
    @ShenyuSofaClient("/batchSaveNameAndId")
    public SofaSimpleTypeBean batchSaveNameAndId(final List<SofaSimpleTypeBean> sofaTestList, final String id, final String name) {
        final String newName = "hello world shenyu sofa param batchSaveAndNameAndId :" + name + ":"
                + sofaTestList.stream()
                .map(SofaSimpleTypeBean::getName)
                .collect(Collectors.joining("-"));
        return new SofaSimpleTypeBean(id, newName);
    }

    @Override
    @ShenyuSofaClient("/saveComplexBean")
    public SofaSimpleTypeBean saveComplexBean(final SofaComplexTypeBean sofaComplexTypeBean) {
        final String id = sofaComplexTypeBean.getIdLists().toString();
        final String typeName = "hello world shenyu sofa param saveComplexBean :" + sofaComplexTypeBean.getSofaSimpleTypeBean().getName();
        return new SofaSimpleTypeBean(id, typeName);
    }

    @Override
    @ShenyuSofaClient("/saveComplexBeanAndName")
    public SofaSimpleTypeBean saveComplexBeanAndName(final SofaComplexTypeBean sofaComplexTypeBean, final String name) {
        final String id = sofaComplexTypeBean.getIdLists().toString();
        final String typeName = "hello world shenyu sofa param saveComplexBeanAndName :" + sofaComplexTypeBean.getSofaSimpleTypeBean().getName() + "-" + name;
        return new SofaSimpleTypeBean(id, typeName);
    }

    @Override
    @ShenyuSofaClient("/saveTwoList")
    public SofaSimpleTypeBean saveTwoList(final List<SofaComplexTypeBean> sofaComplexTypeBeanList, final Map<String, SofaSimpleTypeBean> sofaSimpleTypeBeanMap) {
        SofaSimpleTypeBean simpleTypeBean = new SofaSimpleTypeBean();
        if (!CollectionUtils.isEmpty(sofaComplexTypeBeanList) && !CollectionUtils.isEmpty(sofaSimpleTypeBeanMap)) {
            final SofaComplexTypeBean firstBean = sofaComplexTypeBeanList.get(0);
            final Optional<SofaSimpleTypeBean> firstTypeOptional = sofaSimpleTypeBeanMap.values().stream().findFirst();
            simpleTypeBean.setId(firstBean.getIdLists().toString());
            simpleTypeBean.setName("hello world shenyu sofa param saveTwoList :" + firstBean.getSofaSimpleTypeBean().getName()
                    + "-" + firstTypeOptional.map(SofaSimpleTypeBean::getName));
        }
        return simpleTypeBean;
    }
}
