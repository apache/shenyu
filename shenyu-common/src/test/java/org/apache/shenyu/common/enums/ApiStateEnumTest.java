package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test Cases for ApiStateEnum.
 */
public final class ApiStateEnumTest {

    @Test
    public void testGetState(){

        assertEquals(0, ApiStateEnum.UNPUBLISHED.getState());
        assertEquals(1, ApiStateEnum.PUBLISHED.getState());
        assertEquals(2, ApiStateEnum.OFFLINE.getState());
    }

}