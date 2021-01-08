package org.dromara.soul.admin.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * this is resource Dto.
 *
 * @author nuo-promise
 **/
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ResourceDTO {

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
}
