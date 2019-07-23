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
import org.dromara.soul.web.plugin.config.Singleton;
import org.influxdb.dto.Point;
import org.springframework.data.influxdb.InfluxDBTemplate;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * the influx db Service.
 *
 * @author xiaoyu(Myth)
 */
public class InfluxDbService {

    /**
     * save data in influxDb.
     *
     * @param monitorDO {@linkplain MonitorDO}
     */
    @SuppressWarnings("unchecked")
    public void writeData(final MonitorDO monitorDO) {
        InfluxDBTemplate<Point> influxDBTemplate = Singleton.INST.get(InfluxDBTemplate.class);
        if (Objects.nonNull(influxDBTemplate)) {
            final Point.Builder builder = Point.measurement("monitorDO")
                    .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS);
            builder.tag("host", monitorDO.getHost())
                    .tag("ip", monitorDO.getIp())
                    .tag("method", monitorDO.getMethod())
                    .tag("module", monitorDO.getModule())
                    .tag("resultType", monitorDO.getResultType())
                    .tag("rpcType", monitorDO.getRpcType())
                    .tag("elapsedTime", String.valueOf(monitorDO.getElapsedTime()))
                    .addField("count", monitorDO.getCount());
            final Point point = builder.build();
            influxDBTemplate.write(point);
        }

    }
}
