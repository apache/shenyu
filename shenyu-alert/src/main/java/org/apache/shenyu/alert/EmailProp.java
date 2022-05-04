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

/**
 * Email Prop.
 */
public class EmailProp {

    private String to;

    private String subject;

    private String from;

    private String host;

    private Integer port = 25;

    private Boolean auth = false;

    private Boolean enableTls = false;

    private String userName;

    private String password;

    private String text;

    /**
     * get to.
     * @return to
     */
    public String getTo() {
        return to;
    }

    /**
     * set to.
     * @param to to
     */
    public void setTo(final String to) {
        this.to = to;
    }

    /**
     * get subject.
     * @return subject
     */
    public String getSubject() {
        return subject;
    }

    /**
     * set subject.
     * @param subject subject
     */
    public void setSubject(final String subject) {
        this.subject = subject;
    }

    /**
     * get from.
     * @return from
     */
    public String getFrom() {
        return from;
    }

    /**
     * set from.
     * @param from from
     */
    public void setFrom(final String from) {
        this.from = from;
    }

    /**
     * get host.
     * @return host
     */
    public String getHost() {
        return host;
    }

    /**
     * set host.
     * @param host host
     */
    public void setHost(final String host) {
        this.host = host;
    }

    /**
     * get port default 25.
     * @return port
     */
    public Integer getPort() {
        return port;
    }

    /**
     * set port.
     * @param port port
     */
    public void setPort(final Integer port) {
        this.port = port;
    }

    /**
     * get auth default false.
     * @return auth.
     */
    public Boolean getAuth() {
        return auth;
    }

    /**
     * set auth.
     * @param auth auth
     */
    public void setAuth(final Boolean auth) {
        this.auth = auth;
    }

    /**
     * get enableTls default false.
     * @return enableTls
     */
    public Boolean getEnableTls() {
        return enableTls;
    }

    /**
     * set enableTls.
     * @param enableTls enableTls
     */
    public void setEnableTls(final Boolean enableTls) {
        this.enableTls = enableTls;
    }

    /**
     * get userName.
     * @return userName
     */
    public String getUserName() {
        return userName;
    }

    /**
     * set userName.
     * @param userName userName
     */
    public void setUserName(final String userName) {
        this.userName = userName;
    }

    /**
     * get password.
     * @return password
     */
    public String getPassword() {
        return password;
    }

    /**
     * set password.
     * @param password password
     */
    public void setPassword(final String password) {
        this.password = password;
    }

    /**
     * get text.
     * @return text
     */
    public String getText() {
        return text;
    }

    /**
     * set text.
     * @param text text
     */
    public void setText(final String text) {
        this.text = text;
    }
}
