/*
 *
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.remoting.netty;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandler;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.MessageToMessageDecoder;
import io.netty.handler.codec.MessageToMessageEncoder;
import io.netty.handler.codec.http.*;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.dromara.soul.common.http.HttpMethod;
import org.dromara.soul.common.http.HttpSoulRequest;
import org.dromara.soul.common.http.HttpSoulResponse;
import org.dromara.soul.common.http.HttpStatus;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.codec.Codec;

/**
 * NettyCodec
 * CreateDate: 2019/10/15 15:13
 *
 * @author sixh
 */
@SuppressWarnings("unchecked")
public class NettyCodec implements Codec<FullHttpRequest, FullHttpResponse> {

    private final ChannelHandler encoder = new Encode();

    private final ChannelHandler decoder = new Decode();

    /**
     * Gets encoder.
     *
     * @return the encoder
     */
    ChannelHandler getEncoder() {
        return encoder;
    }

    /**
     * Gets decoder.
     *
     * @return the decoder
     */
    ChannelHandler getDecoder() {
        return decoder;
    }

    @Override
    public FullHttpResponse encode(Channel channel, HttpSoulResponse message) {
        ByteBuf byteBuf = Unpooled.copiedBuffer(message.getBody(), StandardCharsets.UTF_8);
        FullHttpResponse rsp = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.valueOf(message.getStatus()), byteBuf);
        rsp.headers().set(HttpHeaderNames.CONTENT_TYPE, "application/json; charset=UTF-8");
        HttpUtil.setContentLength(rsp, byteBuf.readableBytes());
        return rsp;
    }

    @Override
    public HttpSoulRequest decode(Channel channel, FullHttpRequest fullHttpRequest) {
        HttpSoulRequest request = new HttpSoulRequest();
        request.setUrl(fullHttpRequest.uri());
        String body = fullHttpRequest.content().toString(StandardCharsets.UTF_8);
        request.setBody(body);
        HttpHeaders headers = fullHttpRequest.headers();
        Iterator<Map.Entry<String, String>> entryIterator = headers.iteratorAsString();
        Map<String, String> newHeaders = new HashMap<>(16);
        while (entryIterator.hasNext()) {
            Map.Entry<String, String> next = entryIterator.next();
            newHeaders.put(next.getKey(), next.getValue());
        }
        request.setHeaders(newHeaders);
        request.setMethod(HttpMethod.parse(fullHttpRequest.method().name()));
        request.setStatus(fullHttpRequest.decoderResult().isSuccess() ? HttpStatus.OK : HttpStatus.BAD_REQUEST);
        return request;
    }

    private class Encode extends MessageToMessageEncoder {
        @Override
        protected void encode(ChannelHandlerContext ctx, Object msg, List out) {
            if (msg instanceof HttpSoulResponse) {
                HttpSoulResponse response = (HttpSoulResponse) msg;
                FullHttpResponse rsp = NettyCodec.this.encode(new NettyChannel(ctx.channel()), response);
                out.add(rsp);
            }
        }
    }

    private class Decode extends MessageToMessageDecoder {

        @Override
        protected void decode(ChannelHandlerContext ctx, Object msg, List out) {
            if (msg instanceof FullHttpRequest) {
                FullHttpRequest fullHttpRequest = (FullHttpRequest) msg;
                HttpSoulRequest request = NettyCodec.this.decode(new NettyChannel(ctx.channel()), fullHttpRequest);
                out.add(request);
            }
        }
    }
}
