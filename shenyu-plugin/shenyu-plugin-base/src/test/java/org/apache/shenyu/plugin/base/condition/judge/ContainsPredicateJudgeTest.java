package org.apache.shenyu.plugin.base.condition.judge;

import junit.framework.TestCase;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ContainsPredicateJudgeTest extends TestCase {

    private ConditionData conditionDataForUri;
    private ConditionData conditionDataForIp;

    @Before
    public void setUp() {
        conditionDataForUri = new ConditionData();
        conditionDataForUri.setParamType(ParamTypeEnum.URI.getName());
        conditionDataForUri.setParamValue("/http/**");

        conditionDataForIp = new ConditionData();
        conditionDataForIp.setParamType(ParamTypeEnum.IP.getName());
        conditionDataForIp.setParamValue("127.0.0.1,128.0.0.1");
    }

    @Test
    public void testContainsJudgeForUri() {
        conditionDataForUri.setOperator(OperatorEnum.CONTAINS.getAlias());
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionDataForUri, "/http/**/test"));
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionDataForUri, "/test/http/**"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionDataForUri, "/http1/**"));
    }

    @Test
    public void testContainsJudgeForIp() {
        conditionDataForIp.setOperator(OperatorEnum.CONTAINS.getAlias());
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionDataForIp, "127.0.0.1"));
        Assert.assertTrue(PredicateJudgeFactory.judge(conditionDataForIp, "128.0.0.1"));
        Assert.assertFalse(PredicateJudgeFactory.judge(conditionDataForIp, "0.1.128.0"));
    }

}