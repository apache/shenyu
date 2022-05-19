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

import org.apache.shenyu.admin.model.dto.AlertTemplateDTO;
import org.apache.shenyu.admin.model.entity.AlertTemplateDO;
import org.apache.shenyu.admin.model.vo.AlertTemplateVO;

import java.util.List;

/**
 * Alert template service.
 */
public interface AlertTemplateService {

    /**
     * Add alert template.
     * @param alertTemplateDTO alertTemplateDTO
     * @return row int
     */
    int addTemplate(AlertTemplateDTO alertTemplateDTO);

    /**
     * Delete alert template.
     * @param ids ids
     * @return row int
     */
    int deleteTemplate(List<String> ids);

    /**
     * Update alert template.
     * @param alertTemplateDTO alertTemplateDTO
     * @return row int
     */
    int updateTemplate(AlertTemplateDTO alertTemplateDTO);

    /**
     * Get all template.
     * @return all {@link AlertTemplateVO}
     */
    List<AlertTemplateVO> getAll();

    /**
     * Template detail.
     * @param id id
     * @return {@link AlertTemplateDO}
     */
    AlertTemplateDO detail(Long id);

}
