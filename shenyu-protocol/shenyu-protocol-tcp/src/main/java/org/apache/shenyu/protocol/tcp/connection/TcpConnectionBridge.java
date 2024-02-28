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

package org.apache.shenyu.protocol.tcp.connection;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import reactor.core.Disposable;
import reactor.core.Disposables;
import reactor.netty.Connection;
import reactor.netty.NettyInbound;
import reactor.netty.NettyOutbound;

/**
 * TcpConnectionBridge.
 */
public class TcpConnectionBridge implements Bridge {

    private static final Logger LOG = LoggerFactory.getLogger(TcpConnectionBridge.class);

    @Override
    public void bridge(final Connection server, final Connection client) {
        //   LOG.info("start server#inbound -> client#outbound");
        Disposable requestDisposable = bridge(server.inbound(), client.outbound());
        //  LOG.info("start client#inbound -> server#outbound");
        Disposable responseDisposable = bridge(client.inbound(), server.outbound());
        // binding dispose: when server connection is disposed ,client while close too.
        server.onDispose(Disposables.composite(requestDisposable, responseDisposable, client.channel()::close));
        client.onDispose(Disposables.composite(requestDisposable, responseDisposable, server.channel()::close));
    }

    private Disposable bridge(final NettyInbound inbound, final NettyOutbound outbound) {
        return outbound.send(inbound.receive().retain()).then().subscribe();
    }

}
