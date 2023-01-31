/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.model.vo;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * this is role edit for web front.
 */
public class RoleEditVO implements Serializable {

    private static final long serialVersionUID = -292613858092450065L;

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

    public RoleEditVO() {
    }

    public RoleEditVO(final List<String> rolePermissionList, final RoleVO sysRole, final PermissionInfo allPermissionInfo) {
        this.rolePermissionList = rolePermissionList;
        this.sysRole = sysRole;
        this.allPermissionInfo = allPermissionInfo;
    }

    /**
     * Gets the value of rolePermissionList.
     *
     * @return the value of rolePermissionList
     */
    public List<String> getRolePermissionList() {
        return rolePermissionList;
    }

    /**
     * Sets the rolePermissionList.
     *
     * @param rolePermissionList rolePermissionList
     */
    public void setRolePermissionList(final List<String> rolePermissionList) {
        this.rolePermissionList = rolePermissionList;
    }

    /**
     * Gets the value of sysRole.
     *
     * @return the value of sysRole
     */
    public RoleVO getSysRole() {
        return sysRole;
    }

    /**
     * Sets the sysRole.
     *
     * @param sysRole sysRole
     */
    public void setSysRole(final RoleVO sysRole) {
        this.sysRole = sysRole;
    }

    /**
     * Gets the value of allPermissionInfo.
     *
     * @return the value of allPermissionInfo
     */
    public PermissionInfo getAllPermissionInfo() {
        return allPermissionInfo;
    }

    /**
     * Sets the allPermissionInfo.
     *
     * @param allPermissionInfo allPermissionInfo
     */
    public void setAllPermissionInfo(final PermissionInfo allPermissionInfo) {
        this.allPermissionInfo = allPermissionInfo;
    }

    public static class PermissionInfo {

        /**
         * permission tree list.
         */
        private List<ResourceInfo> treeList;

        /**
         * permission ids.
         */
        private List<String> permissionIds;

        public PermissionInfo() {
        }

        public PermissionInfo(final List<ResourceInfo> treeList, final List<String> permissionIds) {
            this.treeList = treeList;
            this.permissionIds = permissionIds;
        }

        /**
         * Gets the value of treeList.
         *
         * @return the value of treeList
         */
        public List<ResourceInfo> getTreeList() {
            return treeList;
        }

        /**
         * Sets the treeList.
         *
         * @param treeList treeList
         */
        public void setTreeList(final List<ResourceInfo> treeList) {
            this.treeList = treeList;
        }

        /**
         * Gets the value of permissionIds.
         *
         * @return the value of permissionIds
         */
        public List<String> getPermissionIds() {
            return permissionIds;
        }

        /**
         * Sets the permissionIds.
         *
         * @param permissionIds permissionIds
         */
        public void setPermissionIds(final List<String> permissionIds) {
            this.permissionIds = permissionIds;
        }

        /**
         * builder method.
         *
         * @return builder object.
         */
        public static PermissionInfo.PermissionInfoBuilder builder() {
            return new PermissionInfo.PermissionInfoBuilder();
        }

        public static final class PermissionInfoBuilder {

            /**
             * permission tree list.
             */
            private List<ResourceInfo> treeList;

            /**
             * permission ids.
             */
            private List<String> permissionIds;

            public PermissionInfoBuilder() {
            }

            /**
             * treeList.
             *
             * @param treeList the treeList.
             * @return PermissionInfoBuilder.
             */
            public PermissionInfoBuilder treeList(final List<ResourceInfo> treeList) {
                this.treeList = treeList;
                return this;
            }

            /**
             * permissionIds.
             *
             * @param permissionIds the permissionIds.
             * @return PermissionInfoBuilder.
             */
            public PermissionInfoBuilder permissionIds(final List<String> permissionIds) {
                this.permissionIds = permissionIds;
                return this;
            }

            /**
             * build method.
             *
             * @return build object.
             */
            public PermissionInfo build() {
                PermissionInfo permissionInfo = new PermissionInfo(treeList, permissionIds);
                return permissionInfo;
            }
        }
    }

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

        public ResourceInfo() {
        }

        public ResourceInfo(final String id, final String title, final String name, final List<ResourceInfo> children, final Boolean isLeaf, final String parentId) {
            this.id = id;
            this.title = title;
            this.name = name;
            this.children = children;
            this.isLeaf = isLeaf;
            this.parentId = parentId;
        }

        /**
         * Gets the value of id.
         *
         * @return the value of id
         */
        public String getId() {
            return id;
        }

        /**
         * Sets the id.
         *
         * @param id id
         */
        public void setId(final String id) {
            this.id = id;
        }

        /**
         * Gets the value of title.
         *
         * @return the value of title
         */
        public String getTitle() {
            return title;
        }

        /**
         * Sets the title.
         *
         * @param title title
         */
        public void setTitle(final String title) {
            this.title = title;
        }

        /**
         * Gets the value of name.
         *
         * @return the value of name
         */
        public String getName() {
            return name;
        }

        /**
         * Sets the name.
         *
         * @param name name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * Gets the value of children.
         *
         * @return the value of children
         */
        public List<ResourceInfo> getChildren() {
            return children;
        }

        /**
         * Sets the children.
         *
         * @param children children
         */
        public void setChildren(final List<ResourceInfo> children) {
            this.children = children;
        }

        /**
         * Gets the value of isLeaf.
         *
         * @return the value of isLeaf
         */
        public Boolean getIsLeaf() {
            return isLeaf;
        }

        /**
         * Sets the isLeaf.
         *
         * @param isLeaf isLeaf
         */
        public void setIsLeaf(final Boolean isLeaf) {
            this.isLeaf = isLeaf;
        }

        /**
         * Gets the value of parentId.
         *
         * @return the value of parentId
         */
        public String getParentId() {
            return parentId;
        }

        /**
         * Sets the parentId.
         *
         * @param parentId parentId
         */
        public void setParentId(final String parentId) {
            this.parentId = parentId;
        }

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

        /**
         * builder method.
         *
         * @return builder object.
         */
        public static ResourceInfo.ResourceInfoBuilder builder() {
            return new ResourceInfo.ResourceInfoBuilder();
        }

        public static final class ResourceInfoBuilder {

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

            public ResourceInfoBuilder() {
            }

            public ResourceInfoBuilder(final String id, final String title, final String name, final List<ResourceInfo> children, final Boolean isLeaf, final String parentId) {
                this.id = id;
                this.title = title;
                this.name = name;
                this.children = children;
                this.isLeaf = isLeaf;
                this.parentId = parentId;
            }

            /**
             * id.
             *
             * @param id the id.
             * @return ResourceInfoBuilder.
             */
            public ResourceInfoBuilder id(final String id) {
                this.id = id;
                return this;
            }

            /**
             * title.
             *
             * @param title the title.
             * @return ResourceInfoBuilder.
             */
            public ResourceInfoBuilder title(final String title) {
                this.title = title;
                return this;
            }

            /**
             * name.
             *
             * @param name the name.
             * @return ResourceInfoBuilder.
             */
            public ResourceInfoBuilder name(final String name) {
                this.name = name;
                return this;
            }

            /**
             * children.
             *
             * @param children the children.
             * @return ResourceInfoBuilder.
             */
            public ResourceInfoBuilder children(final List<ResourceInfo> children) {
                this.children = children;
                return this;
            }

            /**
             * isLeaf.
             *
             * @param isLeaf the isLeaf.
             * @return ResourceInfoBuilder.
             */
            public ResourceInfoBuilder isLeaf(final Boolean isLeaf) {
                this.isLeaf = isLeaf;
                return this;
            }

            /**
             * parentId.
             *
             * @param parentId the parentId.
             * @return ResourceInfoBuilder.
             */
            public ResourceInfoBuilder parentId(final String parentId) {
                this.parentId = parentId;
                return this;
            }

            /**
             * build method.
             *
             * @return build object.
             */
            public ResourceInfo build() {
                ResourceInfo resourceInfo = new ResourceInfo(id, title, name, children, isLeaf, parentId);
                return resourceInfo;
            }
        }
    }

}
