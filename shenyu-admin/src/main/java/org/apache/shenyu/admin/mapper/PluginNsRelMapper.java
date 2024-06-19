package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.PluginNsRelDO;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.query.PluginQueryCondition;
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
     * @param pluginQuery {@linkplain PluginQuery}
     * @return {@linkplain List}
     */
    List<PluginVO> selectByQuery(PluginQuery pluginQuery);

    /**
     * select plugin by id.
     *
     * @param id primary key.
     * @return {@linkplain PluginVO}
     */
    PluginVO selectById(String id);

    /**
     * search by condition.
     *
     * @param condition condition.
     * @return list
     */
    List<PluginVO> searchByCondition(@Param("condition") PluginQueryCondition condition);
}
