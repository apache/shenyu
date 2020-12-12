package org.dromara.soul.common.enums;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

/**
 * Test Cases for RedisModeEnum.
 *
 * @author YarNeCn
 */
public class RedisModeEnumTest {

    @Test
    public void testAcquireByName() {
        assertEquals(RedisModeEnum.CLUSTER, RedisModeEnum.acquireByName(RedisModeEnum.CLUSTER.getName()));
        assertEquals(RedisModeEnum.SENTINEL, RedisModeEnum.acquireByName(RedisModeEnum.SENTINEL.getName()));
        assertEquals(RedisModeEnum.STANDALONE, RedisModeEnum.acquireByName(RedisModeEnum.STANDALONE.getName()));
        assertNotEquals(RedisModeEnum.STANDALONE, RedisModeEnum.acquireByName(RedisModeEnum.CLUSTER.getName()));

        assertEquals(RedisModeEnum.STANDALONE, RedisModeEnum.acquireByName(null));
        assertEquals(RedisModeEnum.STANDALONE, RedisModeEnum.acquireByName(""));
    }
}
