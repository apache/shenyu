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

package org.apache.shenyu.examples.sofa.api.service;

import org.apache.shenyu.examples.sofa.api.entity.SofaComplexTypeBean;
import org.apache.shenyu.examples.sofa.api.entity.SofaSimpleTypeBean;

import java.util.List;
import java.util.Map;

/**
 * Sofa multi parameter service.
 */
public interface SofaClientMultiParamService {
    
    /**
     * Find by ids and name.
     * body: {"ids":["1232","456"],"name":"hello world"}
     *
     * @param ids  the ids
     * @param name the name
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean findByIdsAndName(List<Integer> ids, String name);
    
    /**
     * Find by array ids and name.
     * body :{"ids":[123,4561],"name":"hello world"}
     *
     * @param ids  the ids
     * @param name the name
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean findByArrayIdsAndName(Integer[] ids, String name);
    
    /**
     * Find by string array.
     * body :{"ids":["1232","456"]}
     *
     * @param ids the ids
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean findByStringArray(String[] ids);
    
    /**
     * Find by list id.
     * body :{"ids":["1232","456"]}
     *
     * @param ids the ids
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean findByListId(List<String> ids);
    
    /**
     * Batch save SofaSimpleTypeBean.
     * body :{"sofaTestList":[{"id":"123","name":"xiaoyu"},{"id":"456","name":"myth"}]}
     *
     * @param sofaTestList the sofa test list
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean batchSave(List<SofaSimpleTypeBean> sofaTestList);
    
    /**
     * Batch save name and id.
     * body: {"sofaTestList":[{"id":"123","name":"xiaoyu"},{"id":"456","name":"myth"}],"id":"789","name":"ttt"}
     *
     * @param sofaTestList the sofa test list.
     * @param id            the id
     * @param name          the name
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean batchSaveNameAndId(List<SofaSimpleTypeBean> sofaTestList, String id, String name);
    
    
    /**
     * Save sofa complex type bean.
     * body : {"sofaSimpleTypeBean":{"id":"123","name":"xiaoyu"},"idLists":["456","789"],"idMaps":{"id2":"2","id1":"1"}}
     *
     * @param sofaComplexTypeBean the sofa complex type bean.
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean saveComplexBean(SofaComplexTypeBean sofaComplexTypeBean);
    
    
    /**
     * Save complex bean test and name dubbo test.
     * body : {"sofaComplexTypeBean":{"sofaSimpleTypeBean":{"id":"123","name":"xiaoyu"},"idLists":["456","789"],"idMaps":{"id2":"2","id1":"1"}},"name":"xiaoyu"}
     *
     * @param sofaComplexTypeBean the sofa complex type bean.
     * @param name            the name
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean saveComplexBeanAndName(SofaComplexTypeBean sofaComplexTypeBean, String name);
    
    /**
     * Save complex bean test and name dubbo test.
     * body : {"sofaComplexTypeBean":[{"sofaSimpleTypeBean":{"id":"123","name":"xiaoyu"},"idLists":["456","789"],"idMaps":{"id2":"2","id1":"1"}}],
     * "sofaSimpleTypeBeanList":[{"id":"123","name":"xiaoyu"}]}
     *
     * @param sofaComplexTypeBeanList the sofa complex type bean.
     * @param sofaSimpleTypeBeanMap            the name
     * @return SofaSimpleTypeBean
     */
    SofaSimpleTypeBean saveTwoList(List<SofaComplexTypeBean> sofaComplexTypeBeanList,
            Map<String, SofaSimpleTypeBean> sofaSimpleTypeBeanMap);
    
}
