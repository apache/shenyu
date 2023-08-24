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

    // number of retries
    public static final String RETRY_ANNOTATION_KEY = "shenyu.apache.org/retry";

    // timeout, in milliseconds
    public static final String TIMEOUT_ANNOTATION_KEY = "shenyu.apache.org/timeout";

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
    public static final String PLUGIN_DUBBO_PATH = "shenyu.apache.org/plugin-dubbo-PATH";

    // The configuration key to specify the Dubbo RPC type for the plugin, in string
    public static final String PLUGIN_DUBBO_RPC_TYPE = "shenyu.apache.org/plugin-dubbo-rpc-type";

    // The configuration key to specify the Dubbo service name for the plugin, in string
    public static final String PLUGIN_DUBBO_SERVICE_NAME = "shenyu.apache.org/plugin-dubbo-service-name";

    // The configuration key to specify the context path for the Dubbo service, in string
    public static final String PLUGIN_DUBBO_CONTEXT_PATH = "shenyu.apache.org/plugin-dubbo-context-path";

    // The configuration key to specify additional RPC extension for the Dubbo plugin, in string
    public static final String PLUGIN_DUBBO_RPC_EXT = "shenyu.apache.org/plugin-dubbo-rpc-ext";

    // The configuration key to specify parameter types for the Dubbo plugin, in string
    public static final String PLUGIN_DUBBO_PARAMENT_TYPE = "shenyu.apache.org/plugin-dubbo-parament-type";
}
