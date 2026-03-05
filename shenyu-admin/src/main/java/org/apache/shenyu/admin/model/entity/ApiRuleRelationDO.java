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

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import org.hibernate.annotations.DynamicUpdate;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.util.Date;

/**
 * ApiRuleRelation.
 */
@DynamicUpdate
@Entity
@Table(name = "api_rule_relation")
@EntityListeners(AuditingEntityListener.class)
public class ApiRuleRelationDO {

    /**
     * primary key id.
     */
    @Id
    private String id;

    /**
     * the table api primary key id.
     */
    private String apiId;

    /**
     * the table rule primary key id.
     */
    private String ruleId;

    /**
     * create time.
     */
    @CreatedDate
    @Column(updatable = false)
    private Date dateCreated;

    /**
     * update time.
     */
    @LastModifiedDate
    private Date dateUpdated;

    /**
     * getId.
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * getApiId.
     * @return apiId.
     */
    public String getApiId() {
        return apiId;
    }

    /**
     * set apiId.
     * @param apiId apiId
     */
    public void setApiId(final String apiId) {
        this.apiId = apiId;
    }

    /**
     * getRuleId.
     * @return ruleId
     */
    public String getRuleId() {
        return ruleId;
    }

    /**
     * setRuleId.
     * @param ruleId ruleId
     */
    public void setRuleId(final String ruleId) {
        this.ruleId = ruleId;
    }

    /**
     * getDateCreated.
     * @return dateCreated.
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * setDateCreated.
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
}
