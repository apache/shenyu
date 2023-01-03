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

package org.apache.shenyu.plugin.logging.clickhouse.constant;

/**
 * clickHouse logging Constant.
 */
public class ClickHouseLoggingConstant {

    /**
     * The constant CREATE_DATABASE_SQL.
     */
    public static final String CREATE_DATABASE_SQL = "create database if not exists `%s`";

    /**
     * The constant CREATE_TABLE_SQL.
     */
    public static final String CREATE_TABLE_SQL = "create table if not exists `%s`.request_log(\n"
            + "    timeLocal   DateTime64,\n"
            + "    clientIp String,\n"
            + "    method  String,\n"
            + "    requestHeader   String,\n"
            + "    responseHeader  String,\n"
            + "    queryParams String,\n"
            + "    requestBody String,\n"
            + "    requestUri  String,\n"
            + "    responseBody    String,\n"
            + "    responseContentLength UInt64,\n"
            + "    rpcType String,\n"
            + "    status  UInt8,\n"
            + "    upstreamIp  String,\n"
            + "    upstreamResponseTime UInt128,\n"
            + "    userAgent String,\n"
            + "    host    String,\n"
            + "    module  String,\n"
            + "    traceId String,\n"
            + "    path    String\n"
            + ") ENGINE = %s()\n"
            + "ORDER BY (timeLocal,clientIp,method,rpcType,upstreamIp,upstreamResponseTime)\n"
            + ";";


    /**
     * The constant CREATE_DISTRIBUTED_TABLE_SQL.
     */
    public static final String CREATE_DISTRIBUTED_TABLE_SQL = "create table if not exists `%s`.request_log_distributed\n"
            + " AS `%s`.request_log ENGINE = Distributed('%s', '%s', 'request_log', rand());";

    /**
     * The constant PRE_INSERT_SQL.
     */
    public static final String PRE_INSERT_SQL = "INSERT INTO `%s`.request_log_distributed"
            + "(timeLocal, clientIp, method, requestHeader, responseHeader, queryParams, "
            + "requestBody, requestUri, responseBody, responseContentLength, rpcType, status, upstreamIp, upstreamResponseTime, userAgent, host, module, traceId, path) "
            + "VALUES "
            + "(:timeLocal, :clientIp,:method, :requestHeader, :responseHeader, :queryParams,"
            + " :requestBody, :requestUri, :responseBody, :responseContentLength, :rpcType, :status, :upstreamIp, :upstreamResponseTime, :userAgent, :host, :module, :traceId, :path);";

}
