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

import jakarta.validation.constraints.NotBlank;
import java.util.Date;
import java.util.Map;

/**
 * Request DTO for /alert/report endpoint.
 * Validation constraints live here so internal callers using AlarmContent directly are unaffected.
 */
public class AlertReportRequest {

    @NotBlank
    private String title;

    private byte level;

    private Map<String, String> labels;

    @NotBlank
    private String content;

    private String namespaceId;

    private Date dateCreated;

    private Date dateUpdated;

    public String getTitle() {
        return title;
    }

    public void setTitle(final String title) {
        this.title = title;
    }

    public byte getLevel() {
        return level;
    }

    public void setLevel(final byte level) {
        this.level = level;
    }

    public Map<String, String> getLabels() {
        return labels;
    }

    public void setLabels(final Map<String, String> labels) {
        this.labels = labels;
    }

    public String getContent() {
        return content;
    }

    public void setContent(final String content) {
        this.content = content;
    }

    public String getNamespaceId() {
        return namespaceId;
    }

    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    public Date getDateCreated() {
        return dateCreated;
    }

    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    public Date getDateUpdated() {
        return dateUpdated;
    }

    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
}
