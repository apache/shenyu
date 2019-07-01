package org.dromara.soul.web.config;

import lombok.Data;

import java.io.Serializable;

/**
 * The type Websocket config.
 *
 * @author xiaoyu(Myth)
 */
@Data
public class WebsocketConfig implements Serializable {

    private String url;

    private Integer delayTime;
}
