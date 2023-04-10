package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class RuleTrieEventEnumTest {

    @Test
    public void testEvent() {
        Arrays.stream(RuleTrieEventEnum.values())
                .forEach(ruleTrieEventEnum -> assertEquals(ruleTrieEventEnum, RuleTrieEventEnum.valueOf(ruleTrieEventEnum.name())));
    }
}
