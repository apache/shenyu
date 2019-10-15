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

package org.dromara.soul.remoting.mina;

import com.google.common.net.HttpHeaders;
import java.util.HashMap;
import java.util.Map;
import org.apache.mina.core.filterchain.IoFilterAdapter;
import org.apache.mina.core.session.IoSession;
import org.apache.mina.core.write.WriteRequest;
import org.apache.mina.http.HttpRequestImpl;
import org.apache.mina.http.api.DefaultHttpResponse;
import org.apache.mina.http.api.HttpStatus;
import org.apache.mina.http.api.HttpVersion;
import org.dromara.soul.common.http.HttpMethod;
import org.dromara.soul.common.http.HttpSoulRequest;
import org.dromara.soul.common.http.HttpSoulResponse;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.codec.Codec;

/**
 * MinaCodec
 * CreateDate: 2019/10/15 17:42
 *
 * @author sixh
 */
public class MinaCodec extends IoFilterAdapter implements Codec<HttpRequestImpl, DefaultHttpResponse> {

    @Override
    public DefaultHttpResponse encode(Channel channel, HttpSoulResponse message) {
        Map<String, String> headers = new HashMap<>(16);
        headers.put(HttpHeaders.CONTENT_TYPE, "application/json; charset=UTF-8");
        headers.put(HttpHeaders.CONTENT_LENGTH, String.valueOf(message.getBody().length()));
        return new DefaultHttpResponse(HttpVersion.HTTP_1_1, HttpStatus.SUCCESS_OK, headers);
    }

    @Override
    public HttpSoulRequest decode(Channel channel, HttpRequestImpl httpRequest) {
        HttpSoulRequest request = new HttpSoulRequest();
        request.setUrl(httpRequest.getRequestPath());
        String body = httpRequest.getQueryString();
        request.setBody(body);
        Map<String, String> headers = httpRequest.getHeaders();
        request.setHeaders(headers);
        request.setMethod(HttpMethod.parse(httpRequest.getMethod().name()));
        request.setStatus(org.dromara.soul.common.http.HttpStatus.OK);
        return request;
    }

    @Override
    public void messageReceived(NextFilter nextFilter, IoSession session, Object message) throws Exception {
        Object msg = message;
        if (message instanceof HttpRequestImpl) {
            HttpRequestImpl request = (HttpRequestImpl) message;
            msg = decode(new MinaChannel(session), request);
        }
        super.messageReceived(nextFilter, session, msg);
    }

    @Override
    public void messageSent(NextFilter nextFilter, IoSession session, WriteRequest writeRequest) throws Exception {
        Object message = writeRequest.getMessage();
        if (message instanceof HttpSoulResponse) {
            HttpSoulResponse response = (HttpSoulResponse) message;
            DefaultHttpResponse newRsp = encode(new MinaChannel(session), response);
            writeRequest.setMessage(newRsp);
        }
        super.messageSent(nextFilter, session, writeRequest);
    }

}
