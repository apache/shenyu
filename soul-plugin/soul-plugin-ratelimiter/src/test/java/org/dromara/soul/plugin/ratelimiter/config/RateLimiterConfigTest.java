package org.dromara.soul.plugin.ratelimiter.config;

import lombok.extern.slf4j.Slf4j;

import org.dromara.soul.common.enums.RedisModeEnum;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * RateLimiterConfig default value test
 * @author wyc192273
 */
@RunWith(MockitoJUnitRunner.class)
@Slf4j
public final class RateLimiterConfigTest {

    private static final int DEFAULT_MAX_IDLE = 8;
    private static final int DEFAULT_MAX_ACTIVE = 8;
    private static final int DEFAULT_MIN_IDLE = 0;

    private RateLimiterConfig rateLimiterConfig;

    @Before
    public void setUp() {
        this.rateLimiterConfig = new RateLimiterConfig();
    }

    /**
     * mode default value test
     */
    @Test
    public void modeDefaultValueTest() {
        Assert.assertEquals(RedisModeEnum.STANDALONE.getName(), rateLimiterConfig.getMode());
    }

    /**
     * maxIdle default value test
     */
    @Test
    public void maxIdleDefaultValueTest() {
        Assert.assertEquals(DEFAULT_MAX_IDLE, rateLimiterConfig.getMaxIdle());
    }

    /**
     * maxActive default value test
     */
    @Test
    public void maxActiveDefaultValueTest() {
        Assert.assertEquals(DEFAULT_MAX_ACTIVE, rateLimiterConfig.getMaxActive());
    }

    /**
     * minIdle default value test
     */
    @Test
    public void minIdleDefaultValueTest() {
        Assert.assertEquals(DEFAULT_MIN_IDLE, rateLimiterConfig.getMinIdle());
    }

    /**
     * equals test
     */
    @Test
    public void equalsTest() {
        RateLimiterConfig defaultConfig = new RateLimiterConfig();
        Assert.assertEquals(defaultConfig, this.rateLimiterConfig);
    }

}