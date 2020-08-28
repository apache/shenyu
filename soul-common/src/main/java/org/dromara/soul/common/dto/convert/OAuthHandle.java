package org.dromara.soul.common.dto.convert;

import lombok.Data;

/**
 * this is OAuth plugin handle.
 *
 * @author zl
 */
@Data
public class OAuthHandle {

    /**
     * access token.
     */
    private String accessToken;

    /**
     * expire.
     */
    private Boolean exipre = false;

    /**
     * expire time.
     */
    private long expireTime;

    /**
     * now - start < expireTime.
     */
    private long startTime;


    public boolean expired() {
        return System.currentTimeMillis() / 1000 - startTime > expireTime;
    }

}
