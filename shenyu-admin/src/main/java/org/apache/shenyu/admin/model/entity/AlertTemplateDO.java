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

package org.apache.shenyu.admin.model.entity;

import java.util.Date;

/**
 * AlertTemplate.
 */
public class AlertTemplateDO {

    /**
     * primary key id.
     */
    private Long id;

    /**
     * alert template name.
     */
    private String name;

    /**
     * alert template strategy name.
     */
    private String strategyName;

    /**
     * alert template content.
     */
    private String content;

    /**
     * create time.
     */
    private Date dateCreated;

    /**
     * update time.
     */
    private Date dateUpdated;

    /**
     * get id.
     * @return id
     */
    public Long getId() {
        return id;
    }

    /**
     * set id.
     * @param id id
     */
    public void setId(final Long id) {
        this.id = id;
    }

    /**
     * get name.
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get strategyName.
     * @return strategyName
     */
    public String getStrategyName() {
        return strategyName;
    }

    /**
     * set strategyName.
     * @param strategyName strategyName
     */
    public void setStrategyName(final String strategyName) {
        this.strategyName = strategyName;
    }

    /**
     * get content.
     * @return content
     */
    public String getContent() {
        return content;
    }

    /**
     * set content.
     * @param content content
     */
    public void setContent(final String content) {
        this.content = content;
    }

    /**
     * get dateCreated.
     * @return dateCreated
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * set dateCreated.
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * get dateUpdated.
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * set dateUpdated.
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

}
