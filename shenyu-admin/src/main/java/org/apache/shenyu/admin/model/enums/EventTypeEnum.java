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

package org.apache.shenyu.admin.model.enums;

import org.apache.shenyu.common.enums.DataEventTypeEnum;

/**
 * EventTypeEnum.
 */
public enum EventTypeEnum {
    
    // ============== created ===================
    /**
     * created event.
     */
    CREATE(DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * register event.
     */
    REGISTER("REGISTER", DataEventTypeEnum.CREATE, "#1f640a"),
    
    /**
     * plugin created event.
     */
    PLUGIN_CREATE("CREATE:Plugin", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * plugin handle created event.
     */
    PLUGIN_HANDLE_CREATE("CREATE:PluginHandle", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * selector created event.
     */
    SELECTOR_CREATE("CREATE:Selector", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * rule created event.
     */
    RULE_CREATE("CREATE:Rule", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * meta data created event.
     */
    META_DATA_CREATE("CREATE:MetaData", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * resource created event.
     */
    RESOURCE_CREATE("CREATE:Resource", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * dict created event.
     */
    DICT_CREATE("CREATE:Dict", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * role created event.
     */
    ROLE_CREATE("CREATE:Role", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    /**
     * user created event.
     */
    USER_CREATE("CREATE:User", DataEventTypeEnum.CREATE, Color.CREATE_COLOR),
    
    // ============== delete ===================
    /**
     * deleted event.
     */
    DELETE(DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * clean event.
     */
    CLEAN(DataEventTypeEnum.DELETE, "#e42c64"),
    
    /**
     * plugin deleted event.
     */
    PLUGIN_DELETE("DELETE:Plugin", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    
    /**
     * plugin handle deleted event.
     */
    PLUGIN_HANDLE_DELETE("DELETE:Plugin-Handle", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * selector deleted event.
     */
    SELECTOR_DELETE("DELETE:Selector", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * rule deleted event.
     */
    RULE_DELETE("DELETE:Rule", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * meta data deleted event.
     */
    META_DATA_DELETE("DELETE:Meta_Data", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * resource deleted event.
     */
    RESOURCE_DELETE("DELETE:Resource", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * dict deleted event.
     */
    DICT_DELETE("DELETE:Dict", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * role deleted event.
     */
    ROLE_DELETE("DELETE:Role", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    /**
     * user deleted event.
     */
    USER_DELETE("DELETE:User", DataEventTypeEnum.DELETE, Color.DELETE_COLOR),
    
    // ============== update ===================
    
    /**
     * update event.
     */
    UPDATE(DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * plugin update.
     */
    PLUGIN_UPDATE("UPDATE:Plugin", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * plugin handle update.
     */
    PLUGIN_HANDLE_UPDATE("UPDATE:PluginHandle", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * selector update.
     */
    SELECTOR_UPDATE("UPDATE:PluginHandle", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * rule update.
     */
    RULE_UPDATE("UPDATE:Rule", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * meta data update.
     */
    META_DATA_UPDATE("UPDATE:MetaData", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * resource update.
     */
    RESOURCE_UPDATE("UPDATE:Resource", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * dict update.
     */
    DICT_UPDATE("UPDATE:Dict", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * role update.
     */
    ROLE_UPDATE("UPDATE:Role", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR),
    
    /**
     * user update.
     */
    USER_UPDATE("UPDATE:User", DataEventTypeEnum.UPDATE, Color.UPDATE_COLOR);
    
    /**
     * type name.
     */
    private final String typeName;
    
    /**
     * type.
     */
    private final DataEventTypeEnum type;
    
    /**
     * color.
     */
    private final String color;
    
    EventTypeEnum(final DataEventTypeEnum type, final String color) {
        this(type.toString(), type, color);
    }
    
    EventTypeEnum(final String typeName, final DataEventTypeEnum type, final String color) {
        this.typeName = typeName;
        this.type = type;
        this.color = color;
    }
    
    /**
     * get typeName.
     *
     * @return type
     */
    public String getTypeName() {
        return typeName;
    }
    
    /**
     * get type.
     *
     * @return DataEventTypeEnum
     */
    public DataEventTypeEnum getType() {
        return type;
    }
    
    
    /**
     * get color.
     *
     * @return color
     */
    public String getColor() {
        return color;
    }
    
    /**
     * default color.
     */
    private static class Color {
        /**
         * default create event color.
         */
        public static final String CREATE_COLOR = "green";
        
        /**
         * default delete event color.
         */
        public static final String DELETE_COLOR = "red";
        
        /**
         * default update event color.
         */
        public static final String UPDATE_COLOR = "#CC6600";
    }
    
}
