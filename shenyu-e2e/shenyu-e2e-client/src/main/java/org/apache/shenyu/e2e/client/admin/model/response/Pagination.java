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

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class Pagination {

    @JsonAlias({"pageNum", "currentPage"})
    private int currentPage;
    
    private int pageSize;
    
    @JsonAlias({"total", "totalSize"})
    private int totalSize;
    
    @JsonAlias({"pages", "totalPage"})
    private int totalPage;
//
//
//
//    private boolean hasNextPage;
//    private int size;
//    private int startRow;
//    private int endRow;
//
//    private int pages;
//    private int pageNum;
//    private int pageSize;

}
