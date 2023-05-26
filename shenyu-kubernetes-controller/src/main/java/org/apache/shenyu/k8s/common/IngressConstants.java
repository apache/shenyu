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
}
