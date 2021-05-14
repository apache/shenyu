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

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import reactor.util.StringUtils;

import java.sql.Timestamp;
import java.util.Optional;

/**
 * The Data Permission Entity.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class DataPermissionDO extends BaseDO {

    private static final long serialVersionUID = 8732493731708038311L;

    /**
     * user id.
     */
    private String userId;

    /**
     * selector or  rule id.
     */
    private String dataId;

    /**
     * selector or rule type： 0: Selector, 1: Rule.
     */
    private Integer dataType;

    /**
     * build Permission DO.
     *
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return {@linkplain DataPermissionDO}
     */
    public static DataPermissionDO buildPermissionDO(final DataPermissionDTO dataPermissionDTO) {
        return Optional.ofNullable(dataPermissionDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            DataPermissionDO dataPermissionDo = DataPermissionDO.builder()
                    .userId(item.getUserId())
                    .dataId(item.getDataId())
                    .dataType(item.getDataType())
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                dataPermissionDo.setId(UUIDUtils.getInstance().generateShortUuid());
                dataPermissionDo.setDateCreated(currentTime);
            } else {
                dataPermissionDo.setId(item.getId());
            }
            return dataPermissionDo;
        }).orElse(null);
    }

    /**
     * build permission do by RuleDO and user id.
     * @param dataId rule id
     * @param userId user id
     * @param dataType data type
     * @return {@linkplain DataPermissionDO}
     */
    public static DataPermissionDO buildCreatePermissionDO(final String dataId, final String userId, final Integer dataType) {

        Timestamp currentTime = new Timestamp(System.currentTimeMillis());

        return DataPermissionDO.builder()
                .userId(userId)
                .dataId(dataId)
                .dataType(dataType)
                .id(UUIDUtils.getInstance().generateShortUuid())
                .dateCreated(currentTime)
                .build();
    }
}
