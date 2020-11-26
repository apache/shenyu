package org.dromara.soul.admin.entity;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.PluginHandleDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.util.Objects;

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
     * 3 indicates select box
     */
    private Integer dataType;

    /**
     *  the attribute type.
     *  1  selector,
     *  2  rule
     */
    private String type;

    /**
     * the attribute sort
     */
    private Integer sort;

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
        pluginHandleDO.setType(pluginHandleDTO.getType());
        pluginHandleDO.setSort(pluginHandleDTO.getSort());
        if (StringUtils.isEmpty(pluginHandleDTO.getId())) {
            pluginHandleDO.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        return pluginHandleDO;
    }
}
