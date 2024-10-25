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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.AlertReceiverQuery;
import org.apache.shenyu.alert.model.AlertReceiverDTO;

import java.util.List;

/**
 * Alert receiver service.
 */
public interface AlertReceiverService {

    /**
     * Add alert receiver.
     *
     * @param alertReceiverDTO alertReceiverDTO
     */
    void addReceiver(AlertReceiverDTO alertReceiverDTO);

    /**
     * Delete alert receiver.
     *
     * @param ids ids
     */
    void deleteReceiver(List<String> ids);

    /**
     * Update alert receiver.
     *
     * @param alertReceiverDTO alertReceiverDTO
     */
    void updateReceiver(AlertReceiverDTO alertReceiverDTO);

    /**
     * Get all receiver.
     * @return all {@link AlertReceiverDTO}
     */
    List<AlertReceiverDTO> getAll();
    
    /**
     * find page of AlertReceiverDTO by query.
     *
     * @param receiverQuery {@linkplain AlertReceiverQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<AlertReceiverDTO> listByPage(AlertReceiverQuery receiverQuery);

    /**
     * receiver detail.
     * @param id id
     * @return {@link AlertReceiverDTO}
     */
    AlertReceiverDTO detail(String id);
    
    /**
     * send test message.
     * @param alertReceiverDTO receiver
     * @return send success or failed
     */
    boolean sendTestMsg(AlertReceiverDTO alertReceiverDTO);
}
