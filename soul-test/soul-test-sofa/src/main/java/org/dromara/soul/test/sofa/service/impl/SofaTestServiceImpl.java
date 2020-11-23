package org.dromara.soul.test.sofa.service.impl;

import org.dromara.soul.client.sofa.common.annotation.SoulSofaClient;
import org.dromara.soul.test.dubbo.api.entity.DubboTest;
import org.dromara.soul.test.dubbo.api.service.DubboTestService;
import org.springframework.stereotype.Service;

import java.util.Random;

/**
 * @author tydhot
 */
@Service("sofaTestService")
public class SofaTestServiceImpl implements DubboTestService {

    @Override
    @SoulSofaClient(path = "/findById", desc = "根据用户查询")
    public DubboTest findById(final String id) {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setId(id);
        dubboTest.setName("hello world Soul Sofa, findById");
        return dubboTest;
    }

    @Override
    @SoulSofaClient(path = "/findAll", desc = "获取所有")
    public DubboTest findAll() {
        DubboTest dubboTest = new DubboTest();
        dubboTest.setName("hello world Soul Sofa , findAll");
        dubboTest.setId(String.valueOf(new Random().nextInt()));
        return dubboTest;
    }

    @Override
    @SoulSofaClient(path = "/insert", desc = "插入一条数据")
    public DubboTest insert(final DubboTest dubboTest) {
        dubboTest.setName("hello world Soul Sofa: " + dubboTest.getName());
        return dubboTest;
    }
}
