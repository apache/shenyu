package org.dromara.soul.web.cache;

import org.dromara.soul.web.config.WebsocketConfig;

/**
 * @author xiaoyu(Myth)
 */
public class WebsocketSyncCache extends AbstractLocalCacheManager {

    private WebsocketConfig websocketConfig;

    public WebsocketSyncCache(WebsocketConfig websocketConfig) {
        this.websocketConfig = websocketConfig;
    }
}
