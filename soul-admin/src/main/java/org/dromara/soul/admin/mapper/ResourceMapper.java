package org.dromara.soul.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.dromara.soul.admin.entity.ResourceDO;
import org.dromara.soul.admin.query.ResourceQuery;

import java.util.List;

/**
 * this is resource mapper.
 *
 * @author nuo-promise
 **/
@Mapper
public interface ResourceMapper {

    /**
     * select resource by id.
     *
     * @param id primary key
     * @return {@linkplain ResourceDO}
     */
    ResourceDO selectById(String id);

    /**
     * select resource by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain List}
     */
    List<ResourceDO> selectByQuery(ResourceQuery resourceQuery);

    /**
     * select resource by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain List}
     */
    List<ResourceDO> findByQuery(ResourceQuery resourceQuery);

    /**
     * count resource by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain Integer}
     */
    Integer countByQuery(ResourceQuery resourceQuery);

    /**
     * insert resource.
     *
     * @param resourceDO {@linkplain ResourceDO}
     * @return rows int
     */
    int insert(ResourceDO resourceDO);

    /**
     * insert selective resource.
     *
     * @param resourceDO {@linkplain ResourceDO}
     * @return rows int
     */
    int insertSelective(ResourceDO resourceDO);

    /**
     * update resource.
     *
     * @param resourceDO {@linkplain ResourceDO}
     * @return rows int
     */
    int update(ResourceDO resourceDO);

    /**
     * update selective resource.
     *
     * @param resourceDO {@linkplain ResourceDO}
     * @return rows int
     */
    int updateSelective(ResourceDO resourceDO);

    /**
     * delete resource.
     *
     * @param id primary key
     * @return rows int
     */
    int delete(String id);

    /**
     * list All.
     *
     * @return {@linkplain List}
     */
    List<ResourceDO> selectAll();
}
