package org.apache.shenyu.common.dto.convert.plugin;

import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * Test case for BrpcRegisterConfig.
 */
public class BrpcRegisterConfigTest {

    @Test
    public void testSetterGetter() {
        BrpcRegisterConfig config = new BrpcRegisterConfig();
        config.setThreadpool("threadPool");
        config.setCorethreads(10);
        config.setThreads(10);
        config.setQueues(2);

        assertThat(config.getThreadpool(), is("threadPool"));
        assertThat(config.getCorethreads(), is(10));
        assertThat(config.getThreads(), is(10));
        assertThat(config.getQueues(), is(2));
    }

    @Test
    public void testEqualsAndHashCode() {
        BrpcRegisterConfig config1 = new BrpcRegisterConfig();
        BrpcRegisterConfig config2 = new BrpcRegisterConfig();

        assertThat(ImmutableSet.of(config1, config2), hasSize(1));
    }


}