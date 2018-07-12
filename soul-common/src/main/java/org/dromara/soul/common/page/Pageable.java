/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *     http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 */

package org.dromara.soul.common.page;

import lombok.Data;

import java.io.Serializable;

/**
 * Pageable.
 * @author xiaoyu(Myth)
 */
@Data
public class Pageable implements Serializable{

	private static final long serialVersionUID = 1L;

	private int showCount = 10;

	private int totalResult = 0;

	private int totalPage = 0;

	private int currentPage = 1;

	private int start;

	private String orderByStr;

	public int getPageStart() {
        return (this.getCurrentPage() - 1) * this.showCount;
    }

	public String getOrderByStr() {
		return orderByStr;
	}

	public void setOrderByStr(String orderByStr) {
		this.orderByStr = orderByStr;
	}

	public int getTotalPage() {
        if (totalResult % showCount == 0){
            totalPage = totalResult / showCount;
        }else{
            totalPage = (totalResult / showCount) + 1;
        }
        return totalPage;
    }

	public int getCurrentPage() {
        if (currentPage <= 0) {
            currentPage = 1;
        }
        return currentPage;
    }
	
	public void setCurrentPage(int currentPage) {
        this.currentPage = currentPage;
    }
}
