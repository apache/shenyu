package org.dromara.soul.test.dubbo.api.service;

import java.util.List;
import java.util.Map;

import org.dromara.soul.test.dubbo.api.entity.ComplexBeanTest;
import org.dromara.soul.test.dubbo.api.entity.DubboTest;

/**
 * @Description
 * @auther feixue
 * @create 2020-04-03 10:38
 */
public interface DubboMultiParamTestService {

    /**
     * find by multi basic type param (String + String) dubbo test
     *
     * @param id
     * @param name
     * @return
     */
    String findDataByParam(String id, String name);


    /**
     * find by multi basic type (Integer + String) dubbo test
     *
     * @param id
     * @param name
     * @return
     */
    String findDataByParam2(Integer id, String name);


    /**
     * find by multi basic type (Integer + Double) dubbo test
     *
     * @param id
     * @param grade
     * @return
     */
    String findDataByParam3(Integer id, Double grade);

    /**
     * find by multi basic type (Integer[] + String) dubbo test
     *
     * @param ids
     * @param name
     * @return
     */
    String findDataByParam4(Integer[] ids, String name);

    /**
     * find by multi basic type (Integer[] + String[]) dubbo test
     * 
     * @param ids
     * @param names
     * @return
     */
    String findDataByParam5(Integer[] ids, String[] names);

    /**
     * find by multi basic type (List<Integer> + List<String>) dubbo test
     * 
     * @param ids
     * @param names
     * @return
     */
    String findDataByParam6(List<Integer> ids, List<String> names);

    /**
     * find by multi basic type (List<Integer> + List<Obect>) dubbo test
     * 
     * @param ids
     * @param dubboTests
     * @return
     */
    String findDataByParam62(List<Integer> ids, List<DubboTest> dubboTests);

    /**
     * find by multi basic type (List<Integer> + Map<String, Integer>) dubbo test
     * 
     * @param ids
     * @param nameMap
     * @return
     */
    String findDataByParam7(List<Integer> ids, Map<String, Integer> nameMap);

    /**
     * find by multi param type(Integer 、javaBean)
     * 
     * @param id
     * @param dubboTest
     * @return
     */
    String findDataByParam8(Integer id, DubboTest dubboTest);


    /**
     * find by multi param type（List<Object>、Map<String,JavaBean>）
     * 
     * @param dubboTests
     * @param complexBeanTestMap
     * @return
     */
    String findDataByParam9(List<DubboTest> dubboTests, Map<String, ComplexBeanTest> complexBeanTestMap);


    /**
     * find by multi param type(Integer、List<String>、Map<String, Integer>、JavaBean)
     * 
     * @param id
     * @param names
     * @param courseMap
     * @param complexBeanTest
     * @return
     */
    String findDataByParam10(Integer id, List<String> names, Map<String, Integer> courseMap,
                             ComplexBeanTest complexBeanTest);

}
