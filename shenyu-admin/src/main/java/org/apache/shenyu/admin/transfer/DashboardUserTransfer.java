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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.vo.DashboardUserEditVO;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;

import java.util.Optional;

/**
 * The interface dashboard user transfer.
 */
public enum DashboardUserTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * conversion data to VO.
     * @param dashboardUserDTO original data
     * @return {@linkplain DashboardUserVO}
     */
    public DashboardUserVO transferDTO2VO(final DashboardUserDTO dashboardUserDTO) {
        return Optional.ofNullable(dashboardUserDTO).map(data -> {
            DashboardUserVO dashboardVO = new DashboardUserVO();
            dashboardVO.setId(data.getId());
            dashboardVO.setUserName(data.getUserName());
            dashboardVO.setPassword(data.getPassword());
            dashboardVO.setRole(data.getRole());
            dashboardVO.setEnabled(data.getEnabled());
            return dashboardVO;
        })
        .orElse(null);
    }

    /**
     * conversion dashboardUserVO to loginDashboardUserVO.
     * @param dashboardUserVO original data
     * @return {@linkplain LoginDashboardUserVO}
     */
    public LoginDashboardUserVO transferVO2LoginVO(final DashboardUserVO dashboardUserVO) {
        return Optional.ofNullable(dashboardUserVO).map(data -> {
            LoginDashboardUserVO dashboardVO = new LoginDashboardUserVO();
            dashboardVO.setId(data.getId());
            dashboardVO.setUserName(data.getUserName());
            dashboardVO.setPassword(data.getPassword());
            dashboardVO.setRole(data.getRole());
            dashboardVO.setEnabled(data.getEnabled());
            dashboardVO.setDateCreated(data.getDateCreated());
            dashboardVO.setDateUpdated(data.getDateUpdated());
            return dashboardVO;
        })
        .orElse(null);
    }

    /**
     * conversion dashboardUserVO to dashboardUserEditVO.
     * @param dashboardUserVO dashboardUserVO
     * @return {@linkplain DashboardUserEditVO}
     */
    public DashboardUserEditVO transfer2EditVO(final DashboardUserVO dashboardUserVO) {
        return Optional.ofNullable(dashboardUserVO).map(data -> {
            DashboardUserEditVO vo = new DashboardUserEditVO();
            vo.setId(data.getId());
            vo.setPassword(data.getPassword());
            vo.setUserName(data.getUserName());
            vo.setRole(data.getRole());
            vo.setEnabled(data.getEnabled());
            vo.setDateCreated(data.getDateCreated());
            vo.setDateUpdated(data.getDateUpdated());
            return vo;
        }).orElse(null);
    }

}
