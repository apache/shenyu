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

package org.apache.shenyu.alert;

import java.util.List;

/**
 * DingTalk Prop.
 */
public class DingTalkProp {

    private String url;

    private List<String> atMobiles;

    private List<String> atUserIds;

    private String msgtype;

    private String content;

    private Boolean isAtAll = false;

    /**
     * get url.
     * @return url
     */
    public String getUrl() {
        return url;
    }

    /**
     * set url.
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * get atMobiles.
     * @return atMobiles
     */
    public List<String> getAtMobiles() {
        return atMobiles;
    }

    /**
     * set atMobiles.
     * @param atMobiles atMobiles
     */
    public void setAtMobiles(final List<String> atMobiles) {
        this.atMobiles = atMobiles;
    }

    /**
     * get atUserIds.
     * @return atUserIds
     */
    public List<String> getAtUserIds() {
        return atUserIds;
    }

    /**
     * set atUserIds.
     * @param atUserIds atUserIds
     */
    public void setAtUserIds(final List<String> atUserIds) {
        this.atUserIds = atUserIds;
    }

    /**
     * get msgtype.
     * @return msgtype
     */
    public String getMsgtype() {
        return msgtype;
    }

    /**
     * set msgtype.
     * @param msgtype msgtype
     */
    public void setMsgtype(final String msgtype) {
        this.msgtype = msgtype;
    }

    /**
     * get content.
     * @return content
     */
    public String getContent() {
        return content;
    }

    /**
     * set content.
     * @param content content
     */
    public void setContent(final String content) {
        this.content = content;
    }

    /**
     * get isAtAll.
     * @return isAtAll
     */
    public Boolean getAtAll() {
        return isAtAll;
    }

    /**
     * set atAll.
     * @param atAll atAll
     */
    public void setAtAll(final Boolean atAll) {
        isAtAll = atAll;
    }
}
