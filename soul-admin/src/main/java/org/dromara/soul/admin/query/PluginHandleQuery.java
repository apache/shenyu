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
     * page parameter.
     */
    private PageParameter pageParameter;
}
