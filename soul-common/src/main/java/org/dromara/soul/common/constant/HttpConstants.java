package org.dromara.soul.common.constant;

import java.util.concurrent.TimeUnit;

/**
 * constants for http, including http long polling
 * @author huangxiaofeng
 * @since 2.0.0
 */
public final class HttpConstants {

    /**
     * Client long polling timeout is 90s
     */
    public static final long CLIENT_POLLING_READ_TIMEOUT = TimeUnit.SECONDS.toMillis( 90 );

    /**
     * The maximum timeout of server block is 60s
     */
    public static final long SERVER_MAX_HOLD_TIMEOUT = TimeUnit.SECONDS.toMillis( 60 );

 }
