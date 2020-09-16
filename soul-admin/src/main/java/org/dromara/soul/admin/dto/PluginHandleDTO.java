package org.dromara.soul.admin.dto;

import java.io.Serializable;
import lombok.Data;

/**
 * this plugin handle from web front.
 * @author liangziqiang
 */
@Data
public class PluginHandleDTO implements Serializable {
    /**
     * primary key.
     */
    private String id;

    /**
     * plugin id.
     */
    private String pluginId;

    /**
     * the attribute name.
     */
    private String field;

    /**
     * the attribute label.
     */
    private String label;

    /**
     * the data type.
     * 1 indicates number
     * 2 indicates string
     */
    private Integer dataType;

}
