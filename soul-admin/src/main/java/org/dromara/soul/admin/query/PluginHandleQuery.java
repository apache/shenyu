package org.dromara.soul.admin.query;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.dromara.soul.admin.page.PageParameter;

/**
 * this is plugin handle query.
 * @author liangziqiang
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PluginHandleQuery {
    /**
     * plugin id.
     */
    private String pluginId;

    /**
     *  type.
     *  1  selector,
     *  2  rule
     */
    private String type;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;
}
