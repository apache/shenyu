package org.apache.shenyu.examples.alibaba.dubbo.service.annotation.impl;

import com.alibaba.dubbo.config.annotation.Service;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.apache.shenyu.examples.dubbo.api.entity.ComplexBeanTest;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.service.DubboMultiParamService;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * The type Dubbo multi param service.
 *
 * @author KevinClair
 **/
@Service
public class DubboMultiParamServiceImpl implements DubboMultiParamService {

    @Override
    @ShenyuDubboClient(path = "/findByIdsAndName", desc = "Query by Ids and name")
    public DubboTest findByIdsAndName(List<Integer> ids, String name) {
        DubboTest test = new DubboTest();
        test.setId(ids.toString());
        test.setName("hello world shenyu alibaba dubbo param findByIdsAndName ：" + name);
        return test;
    }

    @Override
    @ShenyuDubboClient(path = "/findByArrayIdsAndName", desc = "findByArrayIdsAndName")
    public DubboTest findByArrayIdsAndName(Integer[] ids, String name) {
        DubboTest test = new DubboTest();
        test.setId(Arrays.toString(ids));
        test.setName("hello world shenyu alibaba dubbo param findByArrayIdsAndName ：" + name);
        return test;
    }

    @Override
    @ShenyuDubboClient(path = "/findByStringArray", desc = "findByStringArray")
    public DubboTest findByStringArray(String[] ids) {
        DubboTest test = new DubboTest();
        test.setId(Arrays.toString(ids));
        test.setName("hello world shenyu alibaba dubbo param findByStringArray");
        return test;
    }

    @Override
    @ShenyuDubboClient(path = "/findByListId", desc = "findByListId")
    public DubboTest findByListId(List<String> ids) {
        DubboTest test = new DubboTest();
        test.setId(ids.toString());
        test.setName("hello world shenyu alibaba dubbo param findByListId");
        return test;
    }

    @Override
    @ShenyuDubboClient(path = "/batchSave", desc = "batchSave")
    public DubboTest batchSave(List<DubboTest> dubboTestList) {
        DubboTest test = new DubboTest();
        test.setId(dubboTestList.stream().map(DubboTest::getId).collect(Collectors.joining("-")));
        test.setName("hello world shenyu alibaba dubbo param batchSave :" + dubboTestList.stream().map(DubboTest::getName).collect(Collectors.joining("-")));
        return test;
    }

    @Override
    @ShenyuDubboClient(path = "/batchSaveAndNameAndId", desc = "batchSaveAndNameAndId")
    public DubboTest batchSaveAndNameAndId(List<DubboTest> dubboTestList, String id, String name) {
        DubboTest test = new DubboTest();
        test.setId(id);
        test.setName("hello world shenyu alibaba dubbo param batchSaveAndNameAndId :" + name + ":" + dubboTestList.stream().map(DubboTest::getName).collect(Collectors.joining("-")));
        return test;
    }

    @Override
    @ShenyuDubboClient(path = "/saveComplexBeanTest", desc = "saveComplexBeanTest")
    public DubboTest saveComplexBeanTest(ComplexBeanTest complexBeanTest) {
        DubboTest test = new DubboTest();
        test.setId(complexBeanTest.getIdLists().toString());
        test.setName("hello world shenyu alibaba dubbo param saveComplexBeanTest :" + complexBeanTest.getDubboTest().getName());
        return test;
    }

    @Override
    @ShenyuDubboClient(path = "/saveComplexBeanTestAndName", desc = "saveComplexBeanTestAndName")
    public DubboTest saveComplexBeanTestAndName(ComplexBeanTest complexBeanTest, String name) {
        DubboTest test = new DubboTest();
        test.setId(complexBeanTest.getIdLists().toString());
        test.setName("hello world shenyu alibaba dubbo param saveComplexBeanTestAndName :" + complexBeanTest.getDubboTest().getName() + "-" + name);
        return test;
    }
}
