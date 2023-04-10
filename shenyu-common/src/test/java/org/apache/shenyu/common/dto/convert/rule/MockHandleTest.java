package org.apache.shenyu.common.dto.convert.rule;

import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;

/**
 * Test case for MockHandle.
 */
public class MockHandleTest {

    @Test
    public void testSetterGetter() {
        MockHandle handle = new MockHandle();
        handle.setHttpStatusCode(200);
        handle.setResponseContent("OK");

        assertThat(handle.getHttpStatusCode(), is(200));
        assertThat(handle.getResponseContent(), is("OK"));
    }

    @Test
    public void testEqualsAndHashCode() {
        MockHandle handle1 = new MockHandle();
        MockHandle handle2 = new MockHandle();

        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }

}