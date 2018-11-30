/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.influxdb.service;

import org.dromara.soul.web.influxdb.entity.MonitorDO;
import org.influxdb.dto.Point;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.influxdb.InfluxDBTemplate;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * the influx db Service.
 *
 * @author xiaoyu(Myth)
 */
@Component
@SuppressWarnings("unchecked")
public class InfluxDbService {

    private final InfluxDBTemplate influxDBTemplate;

    /**
     * Instantiates a new Influx db service.
     *
     * @param influxDBTemplate the influx db template
     */
    @Autowired
    public InfluxDbService(final InfluxDBTemplate influxDBTemplate) {
        this.influxDBTemplate = influxDBTemplate;
    }

    /**
     * save data in influxDb.
     *
     * @param monitorDO {@linkplain MonitorDO}
     */
    public void writeData(final MonitorDO monitorDO) {
        final Point.Builder builder = Point.measurement("monitorDO")
                .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS);
        builder.tag("host", monitorDO.getHost())
                .tag("ip", monitorDO.getIp())
                .tag("method", monitorDO.getMethod())
                .tag("module", monitorDO.getModule())
                .tag("resultType", monitorDO.getResultType())
                .tag("rpcType", monitorDO.getRpcType())
                .addField("count", monitorDO.getCount());
        final Point point = builder.build();
        influxDBTemplate.write(point);
    }
}
