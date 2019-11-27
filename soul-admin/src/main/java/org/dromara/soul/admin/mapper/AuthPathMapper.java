package org.dromara.soul.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.dromara.soul.admin.entity.AuthPathDO;

import java.util.List;

/**
 * The interface Auth path mapper.
 *
 * @author xiaoyu
 */
@Mapper
public interface AuthPathMapper {

    /**
     * Save int.
     *
     * @param authPathDO the auth resource do
     * @return the int
     */
    int save(AuthPathDO authPathDO);

    /**
     * Batch save int.
     *
     * @param authPathDOList the auth path do list
     * @return the int
     */
    int batchSave(@Param("authPathDOList") List<AuthPathDO> authPathDOList);

    /**
     * Update int.
     *
     * @param authPathDO the auth resource do
     * @return the int
     */
    int update(AuthPathDO authPathDO);

    /**
     * Find by auth id list.
     *
     * @param authId the auth id
     * @return the list
     */
    List<AuthPathDO> findByAuthId(String authId);

    /**
     * Find by auth id and app name list.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the list
     */
    List<AuthPathDO> findByAuthIdAndAppName(@Param("authId") String authId, @Param("appName") String appName);


    /**
     * Delete by auth id and app name int.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the int
     */
    int deleteByAuthIdAndAppName(@Param("authId") String authId, @Param("appName") String appName);


    /**
     * Delete by auth id int.
     *
     * @param authId the auth id
     * @return the int
     */
    int deleteByAuthId(@Param("authId") String authId);

    /**
     * Batch update status int.
     *
     * @param status the status
     * @param ids    the ids
     * @return the int
     */
    int batchUpdateStatus(Boolean status, List<String> ids);
}
