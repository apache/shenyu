package org.dromara.soul.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.dromara.soul.admin.entity.PermissionDO;
import org.dromara.soul.admin.query.PermissionQuery;

import java.util.List;

/**
 * this is permission mapper.
 *
 * @author nuo-promise
 **/
@Mapper
public interface PermissionMapper {

    /**
     * select permission by id.
     *
     * @param id primary key.
     * @return {@linkplain PermissionDO}
     */
    PermissionDO selectById(String id);

    /**
     * find by Object id.
     *
     * @param objectId role or user id
     * @return {@linkplain List}
     */
    List<PermissionDO> findByObjectId(String objectId);

    /**
     * insert permission.
     *
     * @param userRoleDO {@linkplain PermissionDO}
     * @return rows int
     */
    int insert(PermissionDO userRoleDO);

    /**
     * insert selective permission.
     *
     * @param permissionDO {@linkplain PermissionDO}
     * @return rows int
     */
    int insertSelective(PermissionDO permissionDO);

    /**
     * delete permission.
     *
     * @param id primary key
     * @return rows int
     */
    int delete(String id);

    /**
     * delete permission by object id and resource id.
     *
     * @param permissionQuery permission query info
     * @return rows int
     */
    int deleteByObjectIdAndResourceId(PermissionQuery permissionQuery);


    /**
     * delete permission by object id.
     *
     * @param objectId object id
     * @return rows int
     */
    int deleteByObjectId(String objectId);

    /**
     * list All.
     *
     * @return {@linkplain List}
     */
    List<PermissionDO> selectAll();
}
