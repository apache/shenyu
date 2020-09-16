package org.dromara.soul.admin.service.impl;

import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.PluginHandleDTO;
import org.dromara.soul.admin.entity.PluginHandleDO;
import org.dromara.soul.admin.mapper.PluginHandleMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.page.PageResultUtils;
import org.dromara.soul.admin.query.PluginHandleQuery;
import org.dromara.soul.admin.service.PluginHandleService;
import org.dromara.soul.admin.vo.PluginHandleVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * PluginHandleServiceImpl.
 *
 * @author liangziqiang.
 */
@Service("pluginHandleService")
public class PluginHandleServiceImpl implements PluginHandleService {

    private final PluginHandleMapper pluginHandleMapper;

    @Autowired(required = false)
    public PluginHandleServiceImpl(final PluginHandleMapper pluginHandleMapper) {
        this.pluginHandleMapper = pluginHandleMapper;
    }

    @Override
    public CommonPager<PluginHandleVO> listByPage(final PluginHandleQuery pluginHandleQuery) {
        PageParameter pageParameter = pluginHandleQuery.getPageParameter();
        Integer count = pluginHandleMapper.countByQuery(pluginHandleQuery);
        return PageResultUtils.result(pageParameter, count, () -> pluginHandleMapper.selectByQuery(pluginHandleQuery).stream().map(PluginHandleVO::buildPluginHandleVO).collect(Collectors.toList()));
    }

    @Override
    public Integer createOrUpdate(final PluginHandleDTO pluginHandleDTO) {
        int pluginHandleCount;
        PluginHandleDO pluginHandleDO = PluginHandleDO.buildPluginHandleDO(pluginHandleDTO);
        if (StringUtils.isEmpty(pluginHandleDTO.getId())) {
            pluginHandleCount = pluginHandleMapper.insertSelective(pluginHandleDO);
        } else {
            pluginHandleCount = pluginHandleMapper.updateByPrimaryKeySelective(pluginHandleDO);
        }
        return pluginHandleCount;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer deletePluginHandles(final List<String> ids) {
        int affectedRows = 0;
        for (String id : ids) {
            affectedRows += pluginHandleMapper.delete(id);
        }
        return affectedRows;
    }

    @Override
    public PluginHandleVO findById(final String id) {
        return PluginHandleVO.buildPluginHandleVO(pluginHandleMapper.selectById(id));
    }

    @Override
    public List<PluginHandleVO> list(final String pluginId) {
        PluginHandleQuery pluginHandleQuery = new PluginHandleQuery();
        pluginHandleQuery.setPluginId(pluginId);
        return pluginHandleMapper.selectByQuery(pluginHandleQuery).stream()
            .map(PluginHandleVO::buildPluginHandleVO).collect(Collectors.toList());
    }

}
