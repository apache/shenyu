package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test Cases for TrieMatchModeEnum.
 */
public final class TrieMatchModeEnumTest {

    @Test
    public void testGetMatchMode(){

        assertEquals("antPathMatch", TrieMatchModeEnum.ANT_PATH_MATCH.getMatchMode());
        assertEquals("pathPattern", TrieMatchModeEnum.PATH_PATTERN.getMatchMode());
    }

    @Test
    public void testAcquireTrieMatch(){

        assertEquals(TrieMatchModeEnum.ANT_PATH_MATCH, TrieMatchModeEnum.acquireTrieMatch("antPathMatch"));
        assertEquals(TrieMatchModeEnum.PATH_PATTERN, TrieMatchModeEnum.acquireTrieMatch("pathPattern"));
    }

    @Test
    public void testAcquireTrieMatchException(){

        assertThrows(IllegalArgumentException.class, () -> TrieMatchModeEnum.acquireTrieMatch("abc"));
    }

}