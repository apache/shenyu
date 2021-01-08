package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.ResourceDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.ResourceQuery;
import org.dromara.soul.admin.vo.ResourceVO;

import java.util.List;

/**
 * this is Resource Service.
 *
 * @author nuo-promise
 **/
public interface ResourceService {

    /**
     * create or update resource.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return rows int
     */
    int createOrUpdate(ResourceDTO resourceDTO);

    /**
     * delete resource by id.
     *
     * @param ids {@linkplain List}
     * @return rows int
     */
    int delete(List<String> ids);

    /**
     * find by id.
     *
     * @param id resource id
     * @return {@linkplain ResourceVO}
     */
    ResourceVO findById(String id);

    /**
     * find page of resource by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<ResourceVO> listByPage(ResourceQuery resourceQuery);
}
