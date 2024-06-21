package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.PluginNsRelDO;
import org.apache.shenyu.admin.model.query.PluginNamespaceQuery;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.query.PluginNamespaceQueryCondition;
import org.apache.shenyu.admin.model.vo.PluginNamespaceVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * PluginNsRelMapper.
 */
@Mapper
public interface PluginNsRelMapper extends ExistProvider {
    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);


    /**
     * Batch save int.
     *
     * @param pluginNsRelDOList the pluginNsRel do list
     * @return the int
     */
    int batchSave(@Param("pluginNsRelDOList") List<PluginNsRelDO> pluginNsRelDOList);

    /**
     * select plugin by query.
     *
     * @param pluginNamespaceQuery {@linkplain PluginQuery}
     * @return {@linkplain List}
     */
    List<PluginNamespaceVO> selectByQuery(PluginNamespaceQuery pluginNamespaceQuery);

    /**
     * select plugin by pluginId.
     *
     * @param pluginId primary key.
     * @return {@linkplain PluginVO}
     */
    PluginNamespaceVO selectById(String pluginId,String namespaceId);

    /**
     * search by condition.
     *
     * @param condition condition.
     * @return list
     */
    List<PluginNamespaceVO> searchByCondition(@Param("condition") PluginNamespaceQueryCondition condition);

    /**
     * plugin existed.
     *
     * @param name    name
     * @param exclude exclude
     * @return existed
     */
    Boolean nameExistedExclude(@Param("name") Serializable name, @Param("exclude") List<String> exclude, @Param("namespaceId") String namespaceId);


    /**
     * update selective plugin.
     *
     * @param pluginNsRelDO {@linkplain PluginNsRelDO}
     * @return rows int
     */
    int updateSelective(PluginNsRelDO pluginNsRelDO);

    /**
     * select plugin by id.
     *
     * @param pluginIds primary keys.
     * @return {@linkplain PluginNsRelDO}
     */
    List<PluginNamespaceVO> selectByIds(List<String> pluginIds,String namespaceId);


    /**
     * delete plugin.
     *
     * @param ids primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids,String namespaceId);

    /**
     * select all.
     *
     * @param namespaceId namespace id.
     * @return {@linkplain List}
     */
    List<PluginNamespaceVO> selectAll(String namespaceId);

    /**
     * select all.
     *
     * @return {@linkplain List}
     */
    List<PluginNamespaceVO> selectAll();

    /**
     * enable data by a list of ids.
     *
     * @param idList   a list of ids
     * @param enabled status
     * @return the count of enabled datas
     */
    int updateEnableByIdList(@Param("idList") List<String> idList, @Param("enabled") Boolean enabled);
}
