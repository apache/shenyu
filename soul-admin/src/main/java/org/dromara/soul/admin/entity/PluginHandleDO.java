package org.dromara.soul.admin.entity;

import java.util.Objects;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.PluginHandleDTO;
import org.dromara.soul.common.utils.UUIDUtils;

/**
 * plugin handle json definition.
 * @author liangziqiang
 */
@Data
public class PluginHandleDO extends BaseDO {

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

    /**
     * build {@linkplain PluginHandleDO} instance.
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return {@linkplain PluginHandleDO}
     */
    public static PluginHandleDO buildPluginHandleDO(final PluginHandleDTO pluginHandleDTO) {
        if (Objects.isNull(pluginHandleDTO)) {
            return null;
        }

        PluginHandleDO pluginHandleDO = new PluginHandleDO();
        pluginHandleDO.setId(pluginHandleDTO.getId());
        pluginHandleDO.setPluginId(pluginHandleDTO.getPluginId());
        pluginHandleDO.setField(pluginHandleDTO.getField());
        pluginHandleDO.setLabel(pluginHandleDTO.getLabel());
        pluginHandleDO.setDataType(pluginHandleDTO.getDataType());
        if (StringUtils.isEmpty(pluginHandleDTO.getId())) {
            pluginHandleDO.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        return pluginHandleDO;
    }
}
