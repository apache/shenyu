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

package org.apache.shenyu.k8s.common;

public class IngressConstants {

    public static final String K8S_INGRESS_CLASS_ANNOTATION_KEY = "kubernetes.io/ingress.class";

    public static final String SHENYU_INGRESS_CLASS = "shenyu";

    public static final String ID = "1";

    // Load balance type name, refer to LoadBalanceEnum
    public static final String LOADBALANCER_ANNOTATION_KEY = "shenyu.apache.org/loadbalancer";

    //The path parameter inside the handler
    public static final String PATH_ANNOTATION_KEY = "shenyu.apache.org/path";

    // number of retries
    public static final String RETRY_ANNOTATION_KEY = "shenyu.apache.org/retry";

    // timeout, in milliseconds
    public static final String TIMEOUT_ANNOTATION_KEY = "shenyu.apache.org/timeout";

    public static final String ZOOKEEPER_REGISTER_ADDRESS = "shenyu.apache.org/zookeeper-register-address";

    // The maximum length of the request body, in bytes
    public static final String HEADER_MAX_SIZE_ANNOTATION_KEY = "shenyu.apache.org/header-max-size";

    // The maximum length of the request header, in bytes
    public static final String REQUEST_MAX_SIZE_ANNOTATION_KEY = "shenyu.apache.org/request-max-size";

    //Determine if the dubbo plugin is enabled, in bool
    public static final String PLUGIN_DUBBO_ENABLED = "shenyu.apache.org/plugin-dubbo-enabled";

    // The configuration key to specify the Dubbo application name for the plugin, in string
    public static final String PLUGIN_DUBBO_APP_NAME = "shenyu.apache.org/plugin-dubbo-app-name";

    // The configuration key to specify the Dubbo method name for the plugin, in string
    public static final String PLUGIN_DUBBO_METHOD_NAME = "shenyu.apache.org/plugin-dubbo-method-name";

    // The configuration key to specify the Dubbo path for the plugin, in string
    public static final String PLUGIN_DUBBO_PATH = "shenyu.apache.org/plugin-dubbo-path";

    // The configuration key to specify the Dubbo RPC type for the plugin, in string
    public static final String PLUGIN_DUBBO_RPC_TYPE = "shenyu.apache.org/plugin-dubbo-rpc-type";

    // The configuration key to specify the Dubbo service name for the plugin, in string
    public static final String PLUGIN_DUBBO_SERVICE_NAME = "shenyu.apache.org/plugin-dubbo-service-name";

    // The configuration key to specify the context path for the Dubbo service, in string
    public static final String PLUGIN_DUBBO_CONTEXT_PATH = "shenyu.apache.org/plugin-dubbo-context-path";

    //The configuration key to specify the context path for the Dubbo rpc expand, in string
    public static final String PLUGIN_DUBBO_RPC_EXPAND = "shenyu.apache.org/plugin-dubbo-rpc-expand";

    //The configuration key to specify the context path for the Dubbo service name, in string
    public static final String PLUGIN_DUBBO_SREVICE_NAME = "shenyu.apache.org/plugin-dubbo-service-name";

    //The configuration key to specify the context path for the Dubbo service name, in string
    public static final String PLUGIN_DUBBO_PARAMS_TYPE = "shenyu.apache.org/plugin-dubbo-params-type";

    // The configuration key to specify additional RPC extension for the Dubbo plugin, in string
    public static final String PLUGIN_DUBBO_RPC_EXT = "shenyu.apache.org/plugin-dubbo-rpc-ext";

    // The configuration key to specify parameter types for the Dubbo plugin, in string
    public static final String PLUGIN_DUBBO_PARAMENT_TYPE = "shenyu.apache.org/plugin-dubbo-parament-type";

    // The configuration key to specify the Tars path for the plugin, in string
    public static final String PLUGIN_MOTAN_ENABLED = "shenyu.apache.org/plugin-motan-enabled";

    // The configuration key to specify the Tars path for the plugin, in string
    public static final String PLUGIN_MOTAN_CONTEXT_PATH = "shenyu.apache.org/plugin-motan-context-path";

    // The configuration key to specify the Tars path for the plugin, in string
    public static final String PLUGIN_MOTAN_APP_NAME = "shenyu.apache.org/plugin-motan-app-name";

    // The configuration key to specify the Tars method name for the plugin, in string
    public static final String PLUGIN_MOTAN_METHOD_NAME = "shenyu.apache.org/plugin-motan-method-name";

    // The configuration key to specify the Tars path for the plugin, in string
    public static final String PLUGIN_MOTAN_PATH = "shenyu.apache.org/plugin-motan-path";

    // The configuration key to specify the Tars service name for the plugin, in string
    public static final String PLUGIN_MOTAN_SREVICE_NAME = "shenyu.apache.org/plugin-motan-service-name";

    // The configuration key to specify the Tars RPC type for the plugin, in string
    public static final String PLUGIN_MOTAN_RPC_TYPE = "shenyu.apache.org/plugin-motan-rpc-type";

    // The configuration key to specify the Tars parameter types for the plugin, in string
    public static final String PLUGIN_MOTAN_PARAMS_TYPE = "shenyu.apache.org/plugin-motan-params-type";

    // The configuration key to specify additional RPC extension for the Tars plugin, in string
    public static final String PLUGIN_MOTAN_RPC_EXPAND = "shenyu.apache.org/plugin-motan-rpc-expand";

    //Determining whether to use the WebSocket plugin, in String
    public static final String PLUGIN_WEB_SOCKET_ENABLED = "shenyu.apache.org/plugin-web-socket-enabled";

    // Upstream protocol, in string
    public static final String UPSTREAMS_PROTOCOL_ANNOTATION_KEY = "shenyu.apache.org/upstreams-protocol";

    //The configuration key to specify the Context path for the plugin, in string
    public static final String PLUGIN_CONTEXT_PATH_PATH = "shenyu.apache.org/plugin-context-path-path";

    //The configuration key to specify the Context path add prefix for the plugin, in string
    public static final String PLUGIN_CONTEXT_PATH_ADD_PREFIX = "shenyu.apache.org/plugin-context-path-add-prefix";

    public static final String PLUGIN_CONTEXT_PATH_ADD_PREFIXED = "shenyu.apache.org/plugin-context-path-add-prefixed";


    //The configuration key to specify the Brpc application name for the plugin, in string
    public static final String PLUGIN_BRPC_APP_NAME = "shenyu.apache.org/plugin-brpc-app-name";

    //The configuration key to specify the Brpc service name for the plugin, in string
    public static final String PLUGIN_BRPC_SERVICE_NAME = "shenyu.apache.org/plugin-brpc-service-name";

    //The configuration key to specify the Brpc method name for the plugin, in string
    public static final String PLUGIN_BRPC_METHOD_NAME = "shenyu.apache.org/plugin-brpc-method-name";

    //The configuration key to specify the Brpc params type for the plugin, in string
    public static final String PLUGIN_BRPC_PARAMS_TYPE = "shenyu.apache.org/plugin-brpc-params-type";

    //The configuration key to specify the Brpc rpc type for the plugin, in string
    public static final String PLUGIN_BRPC_RPC_TYPE = "shenyu.apache.org/plugin-brpc-rpc-type";

    //The configuration key to specify the Brpc rpc ext for the plugin, in string
    public static final String PLUGIN_BRPC_RPC_EXT = "shenyu.apache.org/plugin-brpc-rpc-ext";

    //The configuration key to specify the Brpc context path for the plugin, in string
    public static final String PLUGIN_BRPC_CONTEXT_PATH = "shenyu.apache.org/plugin-brpc-context-path";

    //The configuration key to specify the Brpc path for the plugin, in string
    public static final String PLUGIN_BRPC_PATH = "shenyu.apache.org/plugin-brpc-path";

    //The configuration key to specify the Brpc enabled for the plugin, in string
    public static final String PLUGIN_BRPC_ENABLED = "shenyu.apache.org/plugin-brpc-enabled";

    //The configuration key to specify the Brpc rpc expand for the plugin, in string
    public static final String PLUGIN_BRPC_RPC_EXPAND = "shenyu.apache.org/plugin-brpc-rpc-expand";

    //The configuration key to specify the Brpc parameter type for the plugin, in string
    public static final String PLUGIN_BRPC_PARAMETER_TYPE = "shenyu.apache.org/plugin-grpc-parameter-type";

    //The configuration key to specify the Grpc application name for the plugin, in string
    public static final String PLUGIN_GRPC_APP_NAME = "shenyu.apache.org/plugin-grpc-app-name";

    //The configuration key to specify the Grpc service name for the plugin, in string
    public static final String PLUGIN_GRPC_SERVICE_NAME = "shenyu.apache.org/plugin-grpc-service-name";

    //The configuration key to specify the Grpc method name for the plugin, in string
    public static final String PLUGIN_GRPC_METHOD_NAME = "shenyu.apache.org/plugin-grpc-method-name";

    //The configuration key to specify the Grpc params type for the plugin, in string
    public static final String PLUGIN_GRPC_PARAMS_TYPE = "shenyu.apache.org/plugin-grpc-params-type";

    //The configuration key to specify the Grpc rpc type for the plugin, in string
    public static final String PLUGIN_GRPC_RPC_TYPE = "shenyu.apache.org/plugin-grpc-rpc-type";

    //The configuration key to specify the Grpc context path for the plugin, in string
    public static final String PLUGIN_GRPC_CONTEXT_PATH = "shenyu.apache.org/plugin-grpc-context-path";

    //The configuration key to specify the Grpc path for the plugin, in string
    public static final String PLUGIN_GRPC_PATH = "shenyu.apache.org/plugin-grpc-path";

    //The configuration key to specify the Grpc enabled for the plugin, in string
    public static final String PLUGIN_GRPC_ENABLED = "shenyu.apache.org/plugin-grpc-enabled";

    //The configuration key to specify the Grpc rpc expand for the plugin, in string
    public static final String PLUGIN_GRPC_RPC_EXPAND = "shenyu.apache.org/plugin-grpc-rpc-expand";

    //The configuration key to specify the Grpc parameter type for the plugin, in string
    public static final String PLUGIN_GRPC_PARAMETER_TYPE = "shenyu.apache.org/plugin-grpc-parameter-type";

    //The configuration key to specify the Sofa application name for the plugin, in string
    public static final String PLUGIN_SOFA_APP_NAME = "shenyu.apache.org/plugin-sofa-app-name";

    //The configuration key to specify the Sofa service name for the plugin, in string
    public static final String PLUGIN_SOFA_SERVICE_NAME = "shenyu.apache.org/plugin-sofa-service-name";

    //The configuration key to specify the Sofa method name for the plugin, in string
    public static final String PLUGIN_SOFA_METHOD_NAME = "shenyu.apache.org/plugin-sofa-method-name";

    //The configuration key to specify the Sofa params type for the plugin, in string
    public static final String PLUGIN_SOFA_PARAMS_TYPE = "shenyu.apache.org/plugin-sofa-params-type";

    //The configuration key to specify the Sofa rpc type for the plugin, in string
    public static final String PLUGIN_SOFA_RPC_TYPE = "shenyu.apache.org/plugin-sofa-rpc-type";

    //The configuration key to specify the Sofa context path for the plugin, in string
    public static final String PLUGIN_SOFA_CONTEXT_PATH = "shenyu.apache.org/plugin-sofa-context-path";

    //The configuration key to specify the Sofa path for the plugin, in string
    public static final String PLUGIN_SOFA_PATH = "shenyu.apache.org/plugin-sofa-path";

    //The configuration key to specify the Sofa enabled for the plugin, in string
    public static final String PLUGIN_SOFA_ENABLED = "shenyu.apache.org/plugin-sofa-enabled";

    //The configuration key to specify the Sofa rpc expand for the plugin, in string
    public static final String PLUGIN_SOFA_RPC_EXPAND = "shenyu.apache.org/plugin-sofa-rpc-expand";

    //The configuration key to specify the Sofa parameter type for the plugin, in string
    public static final String PLUGIN_SOFA_PARAMETER_TYPE = "shenyu.apache.org/plugin-sofa-parameter-type";

    //The configuration key to specify the Sofa parameter type for the plugin, in string
    public static final String PLUGIN_SOFA_RPC_EXT = "shenyu.apache.org/plugin-sofa-rpc-ext";
}
