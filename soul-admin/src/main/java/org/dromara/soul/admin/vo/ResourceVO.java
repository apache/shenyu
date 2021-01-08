package org.dromara.soul.admin.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.dromara.soul.admin.entity.ResourceDO;
import org.dromara.soul.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Optional;

/**
 * this is resource for web front.
 *
 * @author nuo-promise
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ResourceVO implements Serializable {

    /**
     * primary key.
     */
    private String id;

    /**
     * resource parent key.
     */
    private String parentId;

    /**
     * resource title.
     */
    private String title;

    /**
     * resource name.
     */
    private String name;

    /**
     * resource url.
     */
    private String url;

    /**
     * resource component.
     */
    private String component;

    /**
     * resource type.
     */
    private Integer resourceType;

    /**
     * resource sort.
     */
    private Integer sort;

    /**
     * resource icon.
     */
    private String icon;

    /**
     * resource is leaf.
     */
    private Boolean isLeaf;

    /**
     * resource is route.
     */
    private Integer isRoute;

    /**
     * resource perms.
     */
    private String perms;

    /**
     * resource status.
     */
    private Integer status;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build ResourceVO.
     *
     * @param resourceDO {@linkplain ResourceDO}
     * @return {@linkplain ResourceVO}
     */
    public static ResourceVO buildResourceVO(final ResourceDO resourceDO) {
        return Optional.ofNullable(resourceDO).map(item -> new ResourceVO(item.getId(), item.getParentId(),
                item.getTitle(), item.getName(), item.getUrl(), item.getComponent(), item.getResourceType(),
                item.getSort(), item.getIcon(), item.getIsLeaf(), item.getIsRoute(), item.getPerms(), item.getStatus(),
                DateUtils.localDateTimeToString(item.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(item.getDateUpdated().toLocalDateTime()))).orElse(null);
    }
}
