package org.dromara.soul.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.dromara.soul.admin.entity.AuthParamDO;

import java.util.List;

/**
 * The interface Auth param mapper.
 *
 * @author xiaoyu
 */
@Mapper
public interface AuthParamMapper {

    /**
     * Save int.
     *
     * @param authParamDO the auth param do
     * @return the int
     */
    int save(AuthParamDO authParamDO);

    /**
     * Batch save int.
     *
     * @param authParamDOList the auth param do list
     * @return the int
     */
    int batchSave(@Param("authParamDOList") List<AuthParamDO> authParamDOList);

    /**
     * Update int.
     *
     * @param authParamDO the auth param do
     * @return the int
     */
    int update(AuthParamDO authParamDO);

    /**
     * Find by auth id list.
     *
     * @param authId the auth id
     * @return the list
     */
    List<AuthParamDO> findByAuthId(String authId);

    /**
     * Find by auth id and app name auth param do.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the auth param do
     */
    AuthParamDO findByAuthIdAndAppName(@Param("authId") String authId, @Param("appName") String appName);

    /**
     * Delete by auth id int.
     *
     * @param authId the auth id
     * @return the int
     */
    int deleteByAuthId(String authId);
}
