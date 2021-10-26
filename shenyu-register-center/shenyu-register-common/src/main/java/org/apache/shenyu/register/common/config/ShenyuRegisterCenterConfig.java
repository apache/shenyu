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

package org.apache.shenyu.register.common.config;

/**
 * Governance center configuration.
 */
public final class ShenyuRegisterCenterConfig extends PropertiesConfig {
    
    private String registerType;
    
    private String serverLists;

    private Http http = new Http();

    private Dubbo dubbo = new Dubbo();

    private Grpc grpc = new Grpc();

    private Motan motan = new Motan();

    private Sofa sofa = new Sofa();

    private Tars tars = new Tars();

    public ShenyuRegisterCenterConfig() {

    }

    /**
     * getRegisterType.
     *
     * @return String
     */
    public String getRegisterType() {
        return registerType;
    }

    /**
     * setRegisterType.
     *
     * @param registerType registerType
     */
    public void setRegisterType(final String registerType) {
        this.registerType = registerType;
    }

    /**
     * getServerLists.
     *
     * @return String
     */
    public String getServerLists() {
        return serverLists;
    }

    /**
     * setServerLists.
     *
     * @param serverLists serverLists
     */
    public void setServerLists(final String serverLists) {
        this.serverLists = serverLists;
    }

    /**
     * get http.
     *
     * @return the http
     */
    public Http getHttp() {
        return http;
    }

    /**
     * set http.
     *
     * @param http the http
     */
    public void setHttp(final Http http) {
        this.http = http;
    }

    /**
     * get dubbo.
     *
     * @return the dubbo
     */
    public Dubbo getDubbo() {
        return dubbo;
    }

    /**
     * set dubbo.
     *
     * @param dubbo the dubbo
     */
    public void setDubbo(final Dubbo dubbo) {
        this.dubbo = dubbo;
    }

    /**
     * get grpc.
     *
     * @return the grpc
     */
    public Grpc getGrpc() {
        return grpc;
    }

    /**
     * set grpc.
     *
     * @param grpc the grpc
     */
    public void setGrpc(final Grpc grpc) {
        this.grpc = grpc;
    }

    /**
     * get motan.
     *
     * @return the motan
     */
    public Motan getMotan() {
        return motan;
    }

    /**
     * set motan.
     *
     * @param motan the motan
     */
    public void setMotan(final Motan motan) {
        this.motan = motan;
    }

    /**
     * get sofa.
     *
     * @return the sofa
     */
    public Sofa getSofa() {
        return sofa;
    }

    /**
     * set sofa.
     *
     * @param sofa the sofa
     */
    public void setSofa(final Sofa sofa) {
        this.sofa = sofa;
    }

    /**
     * get tars.
     *
     * @return the tars
     */
    public Tars getTars() {
        return tars;
    }

    /**
     * set tars.
     *
     * @param tars the tars
     */
    public void setTars(final Tars tars) {
        this.tars = tars;
    }

    public static class Http extends PropertiesConfig {

    }

    public static class Dubbo extends PropertiesConfig {

    }

    public static class Grpc extends PropertiesConfig {

    }

    public static class Motan extends PropertiesConfig {

    }

    public static class Sofa extends PropertiesConfig {

    }

    public static class Tars extends PropertiesConfig {

    }
}
