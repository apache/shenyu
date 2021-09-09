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

package org.apache.shenyu.plugin.api.request.id;

import org.apache.shenyu.common.enums.ThreadShareDataEnum;
import org.apache.shenyu.common.utils.ThreadShareUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;

/**
 * ShenyuRequestIdWrap.
 */
public final class ShenyuRequestIdWrap {

    /**
     * Gets new request id.
     *
     * @return the new request id
     */
    public static String newRequestId() {
        return SpringBeanUtils.getInstance().getBean(RequestIdGenerator.class).newId();
    }

    /**
     * Gets the request id.
     *
     * @return the the request id
     */
    public static String getRequestId() {
        return ThreadShareUtils.getRemove(ThreadShareDataEnum.REQUEST_ID.getName());
    }
}
