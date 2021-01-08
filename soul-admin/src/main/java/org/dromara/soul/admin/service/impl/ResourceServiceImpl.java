package org.dromara.soul.admin.service.impl;

import org.dromara.soul.admin.dto.ResourceDTO;
import org.dromara.soul.admin.entity.ResourceDO;
import org.dromara.soul.admin.mapper.ResourceMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.page.PageResultUtils;
import org.dromara.soul.admin.query.ResourceQuery;
import org.dromara.soul.admin.service.ResourceService;
import org.dromara.soul.admin.vo.ResourceVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.util.StringUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * this Resource Service Impl.
 *
 * @author nuo-promise
 **/
@Service("resourceService")
public class ResourceServiceImpl implements ResourceService {

    private final ResourceMapper resourceMapper;

    @Autowired(required = false)
    public ResourceServiceImpl(final ResourceMapper resourceMapper) {
        this.resourceMapper = resourceMapper;
    }

    /**
     *  create or update resource.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return rows int
     */
    @Override
    public int createOrUpdate(final ResourceDTO resourceDTO) {
        ResourceDO resourceDO = ResourceDO.buildResourceDO(resourceDTO);
        if (StringUtils.isEmpty(resourceDTO.getId())) {
            return resourceMapper.insertSelective(resourceDO);
        } else {
            return resourceMapper.updateSelective(resourceDO);
        }
    }

    /**
     * delete resource info.
     *
     * @param ids {@linkplain List}
     * @return rows int
     */
    @Override
    public int delete(final List<String> ids) {
        int resourceCount = 0;
        for (String id : ids) {
            resourceCount += resourceMapper.delete(id);
        }
        return resourceCount;
    }

    /**
     * find resource info by id.
     *
     * @param id resource id
     * @return {@linkplain ResourceVO}
     */
    @Override
    public ResourceVO findById(final String id) {
        return ResourceVO.buildResourceVO(resourceMapper.selectById(id));
    }

    /**
     * find page of role by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<ResourceVO> listByPage(final ResourceQuery resourceQuery) {
        PageParameter pageParameter = resourceQuery.getPageParameter();
        Integer count = resourceMapper.countByQuery(resourceQuery);
        return PageResultUtils.result(pageParameter, count, () -> resourceMapper.selectByQuery(resourceQuery).stream().map(ResourceVO::buildResourceVO).collect(Collectors.toList()));
    }
}
