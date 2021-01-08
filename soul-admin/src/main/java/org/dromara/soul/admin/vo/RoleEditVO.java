package org.dromara.soul.admin.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * this is role edit for web front.
 *
 * @author nuo-promise
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RoleEditVO implements Serializable {

    /**
     * role have permission list.
     */
    private List<String> rolePermissionList;

    /**
     * role info.
     */
    private RoleVO sysRole;

    /**
     *  all permission info.
     */
    private PermissionInfo allPermissionInfo;

    @Builder
    @Data
    public static class PermissionInfo {

        /**
         * permission tree list.
         */
        private List<ResourceInfo> treeList;

        /**
         * permission ids.
         */
        private List<String> permissionIds;
    }

    @Builder
    @Data
    public static class ResourceInfo {

        /**
         * resource id.
         */
        private String id;

        /**
         * resource title.
         */
        private String title;

        /**
         * resource name.
         */
        private String name;

        /**
         * resource children.
         */
        private List<ResourceInfo> children;

        /**
         * resource leaf.
         */
        private Boolean isLeaf;

        /**
         * resource parentId.
         */
        private String parentId;

        /**
         * build resource info.
         *
         * @param resourceVO {@linkplain ResourceVO}
         * @return {@linkplain ResourceInfo}
         */
        public static ResourceInfo buildResourceInfo(final ResourceVO resourceVO) {
            return Optional.ofNullable(resourceVO).map(item -> {
                ResourceInfo resourceInfo = ResourceInfo.builder()
                        .id(item.getId())
                        .title(item.getTitle())
                        .name(item.getName())
                        .parentId(item.getParentId())
                        .isLeaf(item.getIsLeaf())
                        .build();
                if (item.getIsLeaf().equals(Boolean.FALSE)) {
                    resourceInfo.setChildren(new ArrayList<>());
                }
                return resourceInfo;
            }).orElse(null);
        }
    }

}
