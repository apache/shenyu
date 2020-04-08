package org.dromara.soul.test.dubbo.service.service;

import java.util.List;
import java.util.Map;

import org.dromara.soul.client.common.annotation.SoulClient;
import org.dromara.soul.test.dubbo.api.entity.ComplexBeanTest;
import org.dromara.soul.test.dubbo.api.entity.DubboTest;
import org.dromara.soul.test.dubbo.api.service.DubboMultiParamTestService;
import org.springframework.stereotype.Service;


/**
 * @Description multi parameter dubbo test
 * @auther feixue
 * @create 2020-04-03 10:39
 */
@Service("dubboMultiParamTestService")
public class DubboMultiParamTestServiceImpl implements DubboMultiParamTestService {

    /**
     * { "id": "1", "name": "test" }
     * 
     * @param id
     * @param name
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam", desc = "根据参数查询数据")
    public String findDataByParam(String id, String name) {
        return "id=" + id + ",name=" + name;
    }

    /**
     * { "ids": 1, "name":"aaa" }
     * 
     * @param id
     * @param name { "ids": 1, "name":"aaa" }@return
     */
    @Override
    @SoulClient(path = "/findDataByParam2", desc = "根据参数查询数据")
    public String findDataByParam2(Integer id, String name) {
        return "id=" + id + ",name=" + name;
    }

    /**
     * { "ids": 1, "grade":99.8 }
     * 
     * @param id
     * @param grade
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam3", desc = "根据参数查询数据")
    public String findDataByParam3(Integer id, Double grade) {
        return "id=" + id + ",grade=" + grade;
    }

    /**
     * { "ids": [1,2,3], "name":"test" }
     * 
     * @param ids
     * @param name
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam4", desc = "根据参数查询数据")
    public String findDataByParam4(Integer[] ids, String name) {
        return ids + "," + name;
    }

    /**
     * { "ids": [1, 2, 3], "name": ["test", "test1", "test2"] }
     * 
     * @param ids
     * @param names
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam5", desc = "根据参数查询数据")
    public String findDataByParam5(Integer[] ids, String[] names) {
        return ids + "," + names;
    }


    /**
     * { "ids": [1, 2, 3], "name": ["test", "test1", "test2"] }
     * 
     * @param ids
     * @param names
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam6", desc = "根据参数查询数据")
    public String findDataByParam6(List<Integer> ids, List<String> names) {
        return ids.toString() + "," + names.toString();
    }

    /**
     * { "ids": [1, 2, 3], "nameMap": { "name": 1, "course": 2 } }
     * 
     * @param ids
     * @param dubboTests
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam62", desc = "根据参数查询数据")
    public String findDataByParam62(List<Integer> ids, List<DubboTest> dubboTests) {
        return ids.toString() + "," + dubboTests.toString();
    }

    /**
     *
     * @param ids
     * @param nameMap
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam7", desc = "根据参数查询数据")
    public String findDataByParam7(List<Integer> ids, Map<String, Integer> nameMap) {
        return ids.toString() + "," + nameMap.values();
    }

    /**
     * { "id": 1, "dubboTest": { "id": "test", "name": "test", "ids":[1,2,3] } }
     * 
     * @param id
     * @param dubboTest
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam8", desc = "根据参数查询数据")
    public String findDataByParam8(Integer id, DubboTest dubboTest) {
        return id + "," + dubboTest.toString();
    }

    /**
     * { "dubboTests": [{ "id": "test", "name": "test", "ids": [1, 2, 3] }], "complexBeanTestMap": {
     * "nameMap": { "idArrays": [1, 2, 3], "idLists": ["test1", "test2", "test3"], "idMaps": { "uid": 1,
     * "age": 10 } } } }
     * 
     * @param dubboTests
     * @param complexBeanTestMap
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam9", desc = "根据参数查询数据")
    public String findDataByParam9(List<DubboTest> dubboTests, Map<String, ComplexBeanTest> complexBeanTestMap) {
        return dubboTests.toString() + "," + complexBeanTestMap.values();
    }

    /**
     * {
     * 	"id": 1,
     * 	"name": ["test1", "test2", "test3"],
     * 	"courseMap": {
     * 		"name": 1,
     * 		"age": 10
     *        },
     * 	"complexBeanTest": {
     * 		"idArrays": [1, 2, 3],
     * 		"idLists": ["test1", "test2", "test3"],
     * 		"idMaps": {
     * 			"uid": 1,
     * 			"age": 10
     *        }
     *
     *    }
     * }
     * @param id
     * @param names
     * @param courseMap
     * @param complexBeanTest
     * @return
     */
    @Override
    @SoulClient(path = "/findDataByParam10", desc = "根据复杂参数查询数据")
    public String findDataByParam10(Integer id, List<String> names, Map<String, Integer> courseMap,
                    ComplexBeanTest complexBeanTest) {
        return id + "," + names.toString() + "," + courseMap.values() + "," + complexBeanTest.toString();
    }

}
