package org.apache.shenyu.plugin.base;

import com.google.common.base.CaseFormat;
import com.google.common.collect.Lists;
import org.apache.commons.lang3.text.WordUtils;
import org.apache.commons.text.CaseUtils;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.plugin.base.condition.judge.*;
import org.apache.shenyu.spi.ExtensionLoader;
import org.codehaus.groovy.util.StringUtil;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.StringUtils;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author leven.chen
 */
public class PredicateJudgeSpiTest {

    private List<String> operators;

    @Before
    public void init() {
        operators = Arrays.stream(OperatorEnum.values())
                .filter(OperatorEnum::getSupport)
                .map(OperatorEnum::getAlias)
                .collect(Collectors.toList());
    }

    @Test
    public void test() {

        for (String operator : operators) {
            if ("=".equals(operator)) {
                operator = "equals";
            }
            PredicateJudge predicateJudge;
            try {
                predicateJudge = ExtensionLoader.getExtensionLoader(PredicateJudge.class).getJoin(operator);
                if (Objects.isNull(predicateJudge)) {
                    throw new RuntimeException();
                }
            } catch (Exception e) {
                throw new UnsupportedOperationException("predicate judge not found by " + operator);
            }
        }

    }
}
