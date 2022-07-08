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

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import org.apache.shenyu.admin.model.page.condition.BaseExcludedSearchCondition;
import org.apache.shenyu.admin.model.page.condition.SearchCondition;
import org.apache.shenyu.common.utils.DateUtils;

import javax.validation.constraints.NotNull;
import java.util.Date;

/**
 * RecordLogQueryCondition.
 */
public class RecordLogQueryCondition extends BaseExcludedSearchCondition implements SearchCondition {
    
    /**
     * search keyword: log context.
     */
    private String keyword;
    
    
    /**
     * log type.
     */
    private String type;
    
    /**
     * start time.
     */
    @NotNull
    @JsonFormat(pattern = DateUtils.DATE_FORMAT_DATETIME)
    private Date startTime;
    
    /**
     * end time.
     */
    @NotNull
    @JsonFormat(pattern = DateUtils.DATE_FORMAT_DATETIME)
    private Date endTime;
    
    /**
     * username.
     */
    @JsonIgnore
    private String username;
    
    @Override
    public void setKeyword(final String keyword) {
        this.keyword = keyword;
    }
    
    @Override
    public String getKeyword() {
        return keyword;
    }
    
    
    /**
     * get startTime.
     *
     * @return time
     */
    public Date getStartTime() {
        return startTime;
    }
    
    /**
     * set startTime.
     *
     * @param startTime startTime
     */
    public void setStartTime(final Date startTime) {
        this.startTime = startTime;
    }
    
    /**
     * get endTime.
     *
     * @return time
     */
    public Date getEndTime() {
        return endTime;
    }
    
    /**
     * set endTime.
     *
     * @param endTime endTime
     */
    public void setEndTime(final Date endTime) {
        this.endTime = endTime;
    }
    
    /**
     * get type.
     *
     * @return type
     */
    public String getType() {
        return type;
    }
    
    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }
    
    /**
     * get username.
     *
     * @return username
     */
    public String getUsername() {
        return username;
    }
    
    /**
     * set username.
     *
     * @param username username
     */
    public void setUsername(final String username) {
        this.username = username;
    }
}
