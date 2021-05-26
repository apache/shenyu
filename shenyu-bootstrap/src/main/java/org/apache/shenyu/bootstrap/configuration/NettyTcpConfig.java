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

package org.apache.shenyu.bootstrap.configuration;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;

@Getter
public class NettyTcpConfig {

    @Value("${netty.tcp.select.count:1}")
    private int selectCount;

    @Value("${netty.tcp.worker.count:4}")
    private int workerCount;

    @Value("${netty.tcp.connect_timeout_millis:10000}")
    private int connectTimeoutMillis;

    @Value("${netty.tcp.write_buffer_high_water_mark:65536}")
    private int writeBufferHighWaterMark;

    @Value("${netty.tcp.write_buffer_low_water_mark:32768}")
    private int writeBufferLowWaterMark;

    @Value("${netty.tcp.so_keepalive:false}")
    private boolean soKeepalive;

    @Value("${netty.tcp.so_reuseaddr:false}")
    private boolean soReuseaddr;

    @Value("${netty.tcp.so_linger:-1}")
    private int soLinger;

    @Value("${netty.tcp.so_backlog:128}")
    private int soBacklog;

    @Value("${netty.tcp.tcp_nodelay:true}")
    private boolean tcpNodelay;
}
