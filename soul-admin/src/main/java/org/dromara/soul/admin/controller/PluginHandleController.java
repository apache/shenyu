package org.dromara.soul.admin.controller;

import java.util.List;
import java.util.Objects;
import org.dromara.soul.admin.dto.PluginHandleDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.PluginHandleQuery;
import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.service.PluginHandleService;
import org.dromara.soul.admin.vo.PluginHandleVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * this is a plugin handle controller.
 * @author liangziqiang.
 */
@RestController
@RequestMapping("/plugin-handle")
public class PluginHandleController {
    private final PluginHandleService pluginHandleService;

    @Autowired(required = false)
    public PluginHandleController(final PluginHandleService pluginHandleService) {
        this.pluginHandleService = pluginHandleService;
    }

    /**
     * query plugin handle by plugin id.
     * @param pluginId  plugin id.
     * @param currentPage  current page.
     * @param pageSize page size
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("")
    public SoulAdminResult queryPluginHandles(final String pluginId, final Integer currentPage, final Integer pageSize) {
        CommonPager<PluginHandleVO> commonPager = pluginHandleService.listByPage(new PluginHandleQuery(pluginId, new PageParameter(currentPage, pageSize)));
        return SoulAdminResult.success("query plugin handle success", commonPager);
    }

    /**
     * query plugin handle by plugin id.
     * @param pluginId  plugin id.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/all/{pluginId}")
    public SoulAdminResult queryAllPluginHandlesByPluginId(@PathVariable("pluginId") final String pluginId) {
        List<PluginHandleVO> pluginHandleVOS = pluginHandleService.list(pluginId);
        return SoulAdminResult.success("query plugin handle success", pluginHandleVOS);
    }

    /**
     * detail rule.
     *
     * @param id rule id.
     * @return {@linkplain SoulAdminResult}
     */
    @GetMapping("/{id}")
    public SoulAdminResult detailRule(@PathVariable("id") final String id) {
        PluginHandleVO pluginHandleVO = pluginHandleService.findById(id);
        return SoulAdminResult.success("detail plugin handle success", pluginHandleVO);
    }

    /**
     * create plugin handle.
     * @param pluginHandleDTO {@link PluginHandleDTO}
     * @return {@link SoulAdminResult}
     */
    @PostMapping("")
    public SoulAdminResult createPluginHandle(@RequestBody final PluginHandleDTO pluginHandleDTO) {
        Integer createCount = pluginHandleService.createOrUpdate(pluginHandleDTO);
        return SoulAdminResult.success("add plugin handle success", createCount);
    }

    /**
     * update plugin handle by id.
     * @param id plugin handle id
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return {@linkplain SoulAdminResult}
     */
    @PutMapping("/{id}")
    public SoulAdminResult updatePluginHandle(@PathVariable("id") final String id, @RequestBody final PluginHandleDTO pluginHandleDTO) {
        Objects.requireNonNull(pluginHandleDTO);
        pluginHandleDTO.setId(id);
        Integer updateCount = pluginHandleService.createOrUpdate(pluginHandleDTO);
        return SoulAdminResult.success("update plugin handle success", updateCount);
    }

    /**
     * batch delete some plugin handles by some id list.
     * @param ids plugin handle id list.
     * @return {@linkplain SoulAdminResult}
     */
    @DeleteMapping("/batch")
    public SoulAdminResult deletePluginHandles(@RequestBody final List<String> ids) {
        Integer deleteCount = pluginHandleService.deletePluginHandles(ids);
        return SoulAdminResult.success("delete plugin handle success", deleteCount);
    }
}
