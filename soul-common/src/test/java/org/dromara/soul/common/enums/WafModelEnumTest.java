package org.dromara.soul.common.enums;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class WafModelEnumTest {
    @Test
    public void testGetName() {
        assertEquals("black", WafModelEnum.BLACK.getName());
        assertEquals("mixed", WafModelEnum.MIXED.getName());
    }
}
