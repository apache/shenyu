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

package org.apache.shenyu.admin.model.query;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.apache.shenyu.admin.model.page.condition.BaseExcludedSearchCondition;
import org.apache.shenyu.admin.model.page.condition.SearchCondition;
import org.apache.shenyu.admin.model.page.condition.SwitchCondition;

import java.util.List;

/**
 * this is rule query condition.
 */
public class SelectorQueryCondition extends BaseExcludedSearchCondition implements SearchCondition, SwitchCondition {
    
    /**
     * search keyword: selector name.
     */
    private String keyword;
    
    /**
     * switch status: selector status[close or open].
     */
    private Boolean switchStatus;
    
    /**
     * selector from plugin.
     */
    private List<String> plugin;
    
    /**
     * user id.
     */
    @JsonIgnore
    private String userId;
    
    /**
     * Gets the value of userId.
     *
     * @return the value of userId
     */
    public String getUserId() {
        return userId;
    }
    
    /**
     * Sets the userId.
     *
     * @param userId userId
     */
    public void setUserId(final String userId) {
        this.userId = userId;
    }
    
    
    /**
     * get switchStatus.
     *
     * @return status
     */
    @Override
    public Boolean getSwitchStatus() {
        return switchStatus;
    }
    
    /**
     * set switchStatus.
     *
     * @param switchStatus status
     */
    public void setSwitchStatus(final Boolean switchStatus) {
        this.switchStatus = switchStatus;
    }
    
    /**
     * get keyword.
     *
     * @return keyword
     */
    @Override
    public String getKeyword() {
        return keyword;
    }
    
    /**
     * set keyword.
     *
     * @param keyword keyword
     */
    @Override
    public void setKeyword(final String keyword) {
        this.keyword = keyword;
    }
    
    /**
     * get plugin.
     *
     * @return plugin ids
     */
    public List<String> getPlugin() {
        return plugin;
    }
    
    /**
     * set plugin.
     *
     * @param plugin plugin ids
     */
    public void setPlugin(final List<String> plugin) {
        this.plugin = plugin;
    }
}
