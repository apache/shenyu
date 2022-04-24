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

package org.apache.shenyu.alert.strategy;

import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import org.apache.shenyu.alert.DingTalkProp;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.spi.Join;

import java.util.HashMap;
import java.util.Map;

/**
 * dingTalk strategy.
 */
@Join
public class DingTalkStrategy implements AlertStrategy {

    @Override
    public void execute(String handle) throws Exception {

        DingTalkProp dingTalkProp = GsonUtils.getInstance().fromJson(handle, DingTalkProp.class);

        HttpResponse execute = HttpRequest.post(dingTalkProp.getUrl()).body(toJson(dingTalkProp)).execute();

        System.err.println(execute.body());

    }

    private String toJson(DingTalkProp prop) {

        Map<String, String> contentMap = new HashMap<>(4);
        contentMap.put("content", prop.getContent());

        Map<String, Object> atMap = new HashMap<>(8);
        atMap.put("atMobiles", prop.getAtMobiles());
        atMap.put("atUserIds", prop.getAtUserIds());
        atMap.put("isAtAll", prop.getAtAll());

        Map<String, Object> body = new HashMap<>(8);

        body.put("at", atMap);
        body.put("msgtype", "text");
        body.put("text", contentMap);
        return GsonUtils.getInstance().toJson(body);
    }

}
