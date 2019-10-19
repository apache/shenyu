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

package org.dromara.soul.store.service.impl;

import com.zaxxer.hikari.HikariDataSource;
import org.dromara.soul.config.api.ConfigEnv;
import org.dromara.soul.config.api.original.SoulDataBase;
import org.dromara.soul.stroe.api.service.RepositoryService;

import javax.sql.DataSource;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Jdbc repository service.
 *
 * @author xiaoyu
 */
public class JdbcRepositoryService extends AbstractJdbcRepositoryService implements RepositoryService {

    private volatile AtomicBoolean isInit = new AtomicBoolean(false);

    @Override
    public void init() {
        if (!isInit.compareAndSet(false, true)) {
            return;
        }
        setDataSource(buildDataSource());
        executeScript();
    }

    private DataSource buildDataSource() {
        SoulDataBase dataBase = ConfigEnv.getInstance().getConfig(SoulDataBase.class);
        HikariDataSource hikariDataSource = new HikariDataSource();
        hikariDataSource.setJdbcUrl(dataBase.getUrl());
        hikariDataSource.setDriverClassName(dataBase.getDrClass());
        hikariDataSource.setUsername(dataBase.getUserName());
        hikariDataSource.setPassword(dataBase.getPassword());
        return hikariDataSource;
    }



}
