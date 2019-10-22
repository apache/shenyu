/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.store.mysql.impl;

import com.zaxxer.hikari.HikariDataSource;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.store.mysql.config.DataBase;
import org.dromara.soul.stroe.api.dto.MetaDataDTO;
import org.dromara.soul.stroe.api.dto.SelectorConditionDTO;
import org.dromara.soul.stroe.api.dto.SelectorDTO;
import org.dromara.soul.stroe.api.service.RepositoryService;

import javax.sql.DataSource;
import java.time.LocalDateTime;

/**
 * The type Jdbc repository service.
 *
 * @author xiaoyu
 */
public class JdbcRepositoryService extends AbstractJdbcRepositoryService implements RepositoryService {

    private static final String INSERT_SELECTOR = "insert into selector "
            + " (id,plugin_id,name,match_mode,"
            + " type,sort,handle,enabled,loged,continued,date_created,date_updated)"
            + " values(?,?,?,?,?,?,?,?,?,?,?,?,?)";

    private static final String INSERT_CONDITION = "insert into selector_condition "
            + " (id,selector_id,param_type,operator,"
            + " param_name,param_value,date_created,date_updated)"
            + " values(?,?,?,?,?,?,?,?,?)";

    private static final String INSERT_META_DATA = "insert into meta_data "
            + " (id,app_name,path,rpc_type,"
            + " service_name,method_name,parameter_types,rpc_ext,date_created,date_updated,enabled)"
            + " values(?,?,?,?,?,?,?,?,?,?,?,?)";

    public JdbcRepositoryService() {
        setDataSource(buildDataSource());
        executeScript();
    }

    @Override
    public int saveSelector(SelectorDTO selectorDTO) {
        int rows = executeUpdate(INSERT_SELECTOR,
                selectorDTO.getId(),
                selectorDTO.getPluginId(),
                selectorDTO.getName(),
                selectorDTO.getMatchMode(),
                selectorDTO.getType(),
                selectorDTO.getSort(),
                selectorDTO.getHandle(),
                selectorDTO.getEnabled(),
                selectorDTO.getLoged(),
                selectorDTO.getContinued(),
                LocalDateTime.now(),
                LocalDateTime.now());
        for (SelectorConditionDTO conditionDTO : selectorDTO.getSelectorConditions()) {
            executeUpdate(INSERT_CONDITION, conditionDTO.getId(), conditionDTO.getSelectorId(),
                    conditionDTO.getParamType(), conditionDTO.getOperator(),
                    conditionDTO.getParamName(), conditionDTO.getParamValue(),
                    LocalDateTime.now(),
                    LocalDateTime.now());
        }
        return rows;
    }

    @Override
    public int saveOrUpdateMetaData(MetaDataDTO metaDataDTO) {
        int rows = 0;
        if (StringUtils.isBlank(metaDataDTO.getId())) {
            rows = executeUpdate(INSERT_META_DATA,
                    metaDataDTO.getId(), metaDataDTO.getAppName(), metaDataDTO.getPath(),
                    metaDataDTO.getRpcType(), metaDataDTO.getServiceName(), metaDataDTO.getMethodName(),
                    metaDataDTO.getParameterTypes(), metaDataDTO.getRpcExt(),
                    LocalDateTime.now(), LocalDateTime.now(), metaDataDTO.getEnabled());
        }
        return rows;
    }

    private DataSource buildDataSource() {
        DataBase dataBase = ConfigEnv.getInstance().getConfig(DataBase.class);
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setJdbcUrl(dataBase.getUrl());
        hikariDataSource.setDriverClassName(dataBase.getDrClass());
        hikariDataSource.setUsername(dataBase.getUserName());
        hikariDataSource.setPassword(dataBase.getPassword());
        return hikariDataSource;
    }
}
