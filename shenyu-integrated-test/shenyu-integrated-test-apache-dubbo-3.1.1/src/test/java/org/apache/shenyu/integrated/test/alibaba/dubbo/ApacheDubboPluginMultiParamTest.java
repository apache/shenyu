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

package org.apache.shenyu.integrated.test.alibaba.dubbo;

import com.google.common.collect.Lists;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.ComplexBeanTest;
import org.apache.shenyu.integratedtest.common.dto.ComplexBeanTestWithNameRequest;
import org.apache.shenyu.integratedtest.common.dto.DubboTest;
import org.apache.shenyu.integratedtest.common.dto.DubboTestListRequest;
import org.apache.shenyu.integratedtest.common.dto.DubboTestListWithIdAndNameRequest;
import org.apache.shenyu.integratedtest.common.dto.IdArrayAndNameRequest;
import org.apache.shenyu.integratedtest.common.dto.IdStringArrayRequest;
import org.apache.shenyu.integratedtest.common.dto.IdStringListRequest;
import org.apache.shenyu.integratedtest.common.dto.IdsAndNameRequest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.hamcrest.MatcherAssert.assertThat;

public class ApacheDubboPluginMultiParamTest extends AbstractPluginDataInit {
    
    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.DUBBO.getName(), "{\"register\":\"zookeeper://shenyu-zk:2181\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testFindByIdsAndName() throws IOException {
        final IdsAndNameRequest idsAndNameRequest = new IdsAndNameRequest();
        idsAndNameRequest.setName("name");
        idsAndNameRequest.setIds(Lists.newArrayList(123, 124, 125));
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/findByIdsAndName", idsAndNameRequest, DubboTest.class);
        assertEquals("hello world shenyu apache dubbo param findByIdsAndName ：name", dubboTest.getName());
        assertEquals("[123, 124, 125]", dubboTest.getId());
    }

    @Test
    public void testFindByArrayIdsAndName() throws IOException {
        final IdArrayAndNameRequest idArrayAndNameRequest = new IdArrayAndNameRequest();
        idArrayAndNameRequest.setName("name");
        idArrayAndNameRequest.setIds(IntStream.of(123, 124, 125).boxed().toArray(Integer[]::new));
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/findByArrayIdsAndName", idArrayAndNameRequest, DubboTest.class);
        assertEquals("hello world shenyu apache dubbo param findByArrayIdsAndName ：name", dubboTest.getName());
        assertEquals("[123, 124, 125]", dubboTest.getId());
    }

    @Test
    public void testFindByStringArray() throws IOException {
        final IdStringArrayRequest idStringArrayRequest = new IdStringArrayRequest();
        idStringArrayRequest.setIds(IntStream.of(123, 124, 125).mapToObj(String::valueOf).toArray(String[]::new));
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/findByStringArray", idStringArrayRequest, DubboTest.class);
        assertEquals("hello world shenyu apache dubbo param findByStringArray", dubboTest.getName());
        assertEquals("[123, 124, 125]", dubboTest.getId());
    }

    @Test
    public void testFindByListId() throws IOException {
        final IdStringListRequest idStringListRequest = new IdStringListRequest();
        idStringListRequest.setIds(Lists.newArrayList("123", "124", "125"));
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/findByListId", idStringListRequest, DubboTest.class);
        assertEquals("hello world shenyu apache dubbo param findByListId", dubboTest.getName());
        assertEquals("[123, 124, 125]", dubboTest.getId());
    }

    @Test
    public void testBatchSave() throws IOException {
        final DubboTestListRequest dubboTestListRequest = new DubboTestListRequest();
        List<DubboTest> dubboTestList = new ArrayList<>();
        dubboTestList.add(new DubboTest("123", "name123"));
        dubboTestList.add(new DubboTest("124", "name124"));
        dubboTestList.add(new DubboTest("125", "name125"));
        dubboTestListRequest.setDubboTestList(dubboTestList);
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/batchSave", dubboTestListRequest, DubboTest.class);
        assertEquals("hello world shenyu apache dubbo param batchSave :name123-name124-name125", dubboTest.getName());
        assertEquals("123-124-125", dubboTest.getId());
    }

    @Test
    public void testBatchSaveAndNameAndId() throws IOException {
        final DubboTestListWithIdAndNameRequest request = new DubboTestListWithIdAndNameRequest();
        request.setId("122");
        request.setName("name122");
        List<DubboTest> dubboTestList = new ArrayList<>();
        dubboTestList.add(new DubboTest("123", "name123"));
        dubboTestList.add(new DubboTest("124", "name124"));
        dubboTestList.add(new DubboTest("125", "name125"));
        request.setDubboTestList(dubboTestList);
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/batchSaveAndNameAndId", request, DubboTest.class);
        assertEquals("hello world shenyu apache dubbo param batchSaveAndNameAndId :name122:name123-name124-name125", dubboTest.getName());
        assertEquals("122", dubboTest.getId());
    }

    @Test
    public void testSaveComplexBeanTest() throws IOException {
        final ComplexBeanTest complexBeanTest = new ComplexBeanTest();
        complexBeanTest.setDubboTest(new DubboTest("122", "name"));
        complexBeanTest.setIdLists(Lists.newArrayList("123", "124"));
        Map<String, String> idMaps = new HashMap<>();
        idMaps.put("key_abc", "value_abc");
        idMaps.put("key_cbd", "value_cbd");
        complexBeanTest.setIdMaps(idMaps);
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/saveComplexBeanTest", complexBeanTest, DubboTest.class);
        assertEquals("hello world shenyu apache dubbo param saveComplexBeanTest :name", dubboTest.getName());
        assertEquals("[123, 124]", dubboTest.getId());
    }

    @Test
    public void testSaveComplexBeanTestAndName() throws IOException {
        final ComplexBeanTestWithNameRequest request = new ComplexBeanTestWithNameRequest();
        ComplexBeanTest complexBeanTest = new ComplexBeanTest();
        complexBeanTest.setDubboTest(new DubboTest("122", "name122"));
        complexBeanTest.setIdLists(Lists.newArrayList("123", "124"));
        Map<String, String> idMaps = new HashMap<>();
        idMaps.put("key_abc", "value_abc");
        idMaps.put("key_cbd", "value_cbd");
        complexBeanTest.setIdMaps(idMaps);
        request.setComplexBeanTest(complexBeanTest);
        request.setName("name");
        DubboTest dubboTest = HttpHelper.INSTANCE.postGateway("/dubbo/saveComplexBeanTestAndName", request, DubboTest.class);
        assertEquals("hello world shenyu alibaba dubbo param saveComplexBeanTestAndName :name122-name", dubboTest.getName());
        assertEquals("[123, 124]", dubboTest.getId());
    }

    @Test
    public void testSaveBigRequestBody() throws IOException {
        DubboTest dubboTest = new DubboTest();
        String id = RandomStringUtils.randomAlphanumeric(2048);
        dubboTest.setId(id);
        String name = RandomStringUtils.randomAlphanumeric(2048);
        dubboTest.setName(name);
        DubboTest dubboTestRet = HttpHelper.INSTANCE.postGateway("/dubbo/bigRequestBody", dubboTest, DubboTest.class);
        assertEquals(id, dubboTestRet.getId());
        assertEquals(name, dubboTestRet.getName());
    }
}
