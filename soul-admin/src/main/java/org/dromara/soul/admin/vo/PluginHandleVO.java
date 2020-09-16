package org.dromara.soul.admin.vo;

import java.io.Serializable;
import java.time.format.DateTimeFormatter;
import java.util.Objects;
import lombok.AllArgsConstructor;
import lombok.Data;
import org.dromara.soul.admin.entity.PluginHandleDO;

/**
 * this is plugin handle view to web front.
 * @author liangziqiang
 */
@Data
@AllArgsConstructor
public class PluginHandleVO implements Serializable {

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
    private String dataType;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build {@linkplain PluginHandleVO}.
     * @param pluginHandleDO {@linkplain PluginHandleDO}
     * @return {@linkplain PluginHandleVO}
     */
    public static PluginHandleVO buildPluginHandleVO(final PluginHandleDO pluginHandleDO) {
        if (Objects.isNull(pluginHandleDO)) {
            return null;
        }
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        return new PluginHandleVO(pluginHandleDO.getId(), pluginHandleDO.getPluginId(),
            pluginHandleDO.getField(), pluginHandleDO.getLabel(),
            String.valueOf(pluginHandleDO.getDataType()), dateTimeFormatter.format(pluginHandleDO.getDateCreated().toLocalDateTime()),
            dateTimeFormatter.format(pluginHandleDO.getDateUpdated().toLocalDateTime()));
    }
}
