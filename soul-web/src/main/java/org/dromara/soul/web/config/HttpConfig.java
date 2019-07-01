package org.dromara.soul.web.config;

import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * @author xiaoyu(Myth)
 */
@Data
public class HttpConfig implements Serializable {

    private List<String> serverList;

    private Integer delayTime;

    private Integer connectionTimeout;
}
