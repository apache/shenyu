package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.PluginHandleDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.PluginHandleQuery;
import org.dromara.soul.admin.vo.PluginHandleVO;

import java.util.List;

/**
 * this is plugin handle service.
 * @author liangziqiang
 */
public interface PluginHandleService {
    /**
     * find page of plugin handle by query.
     * @param pluginHandleQuery {@linkplain PluginHandleQuery}
     * @return {@link CommonPager}
     */
    CommonPager<PluginHandleVO> listByPage(PluginHandleQuery pluginHandleQuery);

    /**
     * create or update plugin handle.
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return affected rows
     */
    Integer createOrUpdate(PluginHandleDTO pluginHandleDTO);

    /**
     * delete plugin handles.
     * @param ids ids to delete
     * @return The number of rows deleted
     */
    Integer deletePluginHandles(List<String> ids);

    /**
     * find plugin handle by id.
     * @param id plugin handle id.
     * @return {@linkplain PluginHandleVO}
     */
    PluginHandleVO findById(String id);

    /**
     * find plugin handle list by plugin id.
     * @param pluginId the plugin id.
     * @param type type 1:selector,2:rule
     * @return plugin handle list.
     */
    List<PluginHandleVO> list(String pluginId, String type);
}
