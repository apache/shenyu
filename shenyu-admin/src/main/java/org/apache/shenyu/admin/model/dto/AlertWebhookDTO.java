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

package org.apache.shenyu.admin.model.dto;

public class AlertWebhookDTO {

    /**
     * primary key id
     */
    private String id;

    /**
     * webhook type
     */
    private Byte type;

    /**
     * webhook name
     */
    private String name;

    /**
     * webhook describe
     */
    private String describe;

    /**
     * key
     */
    private String key;

    /**
     * alert template primary id
     */
    private Long alertTemplateId;

    public String getId() {
        return id;
    }

    public void setId(final String id) {
        this.id = id;
    }

    public Byte getType() {
        return type;
    }

    public void setType(final Byte type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public String getDescribe() {
        return describe;
    }

    public void setDescribe(final String describe) {
        this.describe = describe;
    }

    public String getKey() {
        return key;
    }

    public void setKey(final String key) {
        this.key = key;
    }

    public Long getAlertTemplateId() {
        return alertTemplateId;
    }

    public void setAlertTemplateId(final Long alertTemplateId) {
        this.alertTemplateId = alertTemplateId;
    }
}
