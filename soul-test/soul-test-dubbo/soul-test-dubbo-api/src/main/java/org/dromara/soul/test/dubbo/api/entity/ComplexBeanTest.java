package org.dromara.soul.test.dubbo.api.entity;

import java.util.List;
import java.util.Map;

import lombok.Data;

/**
 * @Description
 * @auther feixue
 * @create 2020-04-01 10:00
 */
@Data
public class ComplexBeanTest {

    private Integer[] idArrays;

    private List<String> idLists;

    private Map<String, Integer> idMaps;
}
