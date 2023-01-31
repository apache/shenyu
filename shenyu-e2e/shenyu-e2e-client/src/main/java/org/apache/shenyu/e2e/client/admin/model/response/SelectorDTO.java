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

package org.apache.shenyu.e2e.client.admin.model.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import org.apache.shenyu.e2e.client.admin.model.data.Condition;

import java.util.Date;
import java.util.List;

@Data
public class SelectorDTO implements ResourceDTO {
    
    private String id;
    
    private String pluginId;
    
    private String name;
    private int matchMode;
    private String matchModeName;
    private int type;
    private String typeName;
    
    private int sort;
    private boolean enabled;
    @JsonProperty(value = "loged")
    private boolean logged;
    private boolean continued;
    
    private String handle;
    
    @JsonProperty("selectorConditions")
    private List<Condition> conditionList;
    
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
    private Date dateCreated;
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
    private Date dateUpdated;
    
}
