package org.apache.shenyu.examples.alibaba.dubbo.service.annotation.impl;

import com.alibaba.dubbo.config.annotation.Service;
import org.apache.shenyu.client.dubbo.common.annotation.ShenyuDubboClient;
import org.apache.shenyu.examples.dubbo.api.entity.DubboTest;
import org.apache.shenyu.examples.dubbo.api.entity.ListResp;
import org.apache.shenyu.examples.dubbo.api.service.DubboTestService;

import java.util.Arrays;
import java.util.Random;

/**
 * The type Dubbo service.
 *
 * @author KevinClair
 **/
@Service
public class DubboTestServiceImpl implements DubboTestService {

    @Override
    @ShenyuDubboClient(path = "/findById", desc = "Query by Id")
    public DubboTest findById(final String id) {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setId(id);
        dubboTest.setName("hello world shenyu Alibaba Dubbo, findById");
        return dubboTest;
    }

    @Override
    @ShenyuDubboClient(path = "/findAll", desc = "Get all data")
    public DubboTest findAll() {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setName("hello world shenyu Alibaba Dubbo , findAll");
        dubboTest.setId(String.valueOf(new Random().nextInt()));
        return dubboTest;
    }

    @Override
    @ShenyuDubboClient(path = "/insert", desc = "Insert a row of data")
    public DubboTest insert(final DubboTest dubboTest) {
        dubboTest.setName("hello world shenyu Alibaba Dubbo: " + dubboTest.getName());
        return dubboTest;
    }

    @Override
    @ShenyuDubboClient(path = "/findList", desc = "Find list")
    public ListResp findList() {
        ListResp listResp = new ListResp();
        listResp.setTotal(1);
        listResp.setUsers(Arrays.asList(new DubboTest("1", "test")));
        return listResp;
    }
}
