package org.dromara.soul.admin.mapper;

import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.dromara.soul.admin.entity.PluginHandleDO;
import org.dromara.soul.admin.query.PluginHandleQuery;

/**
 * The interface Plugin handle mapper.
 * @author liangziqiang
 */
@Mapper
public interface PluginHandleMapper {

    /**
     * Select plugin handle by id.
     * @param id the id.
     * @return the plugin handle do.
     */
    PluginHandleDO selectById(@Param("id") String id);

    /**
     * find plugin handle do list by plugin id.
     * @param pluginId the pluginId
     * @return the list
     */
    List<PluginHandleDO> findByPluginId(@Param("pluginId") String pluginId);

    /**
     * insert plugin handle.
     * @param record {@link PluginHandleDO}
     * @return affected rows
     */
    int insert(PluginHandleDO record);

    /**
     * insert selective plugin handle.
     * @param record {@link PluginHandleDO}
     * @return affected rows.
     */
    int insertSelective(PluginHandleDO record);

    /**
     * count plugin handle by query.
     * @param pluginHandleQuery {@linkplain PluginHandleQuery}
     * @return the count
     */
    int countByQuery(PluginHandleQuery pluginHandleQuery);

    /**
     * select plugin handle list by query.
     * @param pluginHandleQuery {@linkplain PluginHandleQuery}
     * @return the plugin handle list
     */
    List<PluginHandleDO> selectByQuery(PluginHandleQuery pluginHandleQuery);

    /**
     * update some selective columns in plugin_handle.
     * @param record {@linkplain PluginHandleDO}
     * @return affected rows
     */
    int updateByPrimaryKeySelective(PluginHandleDO record);

    /**
     * update plugin handle by primary key.
     * @param record {@linkplain PluginHandleDO}
     * @return affected rows.
     */
    int updateByPrimaryKey(PluginHandleDO record);

    /**
     * delete string id.
     * @param id plugin handle id
     * @return affected rows
     */
    int delete(String id);
}
