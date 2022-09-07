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

package org.apache.shenyu.sdk.starter.core;


import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Request Builder for an HTTP Target.
 * <p>
 * This class is a variation on a UriTemplate, where, in addition to the uri, Headers and Query
 * information also support template expressions.
 * </p>
 */
public final class RequestTemplate implements Serializable {

  private String url;

  private String path;

  private ShenyuRequest.HttpMethod httpMethod;

  private List<ParamMetadata> paramMetadataList;

  private Map<String, Collection<String>> headers = new HashMap<>();

  private ShenyuRequest.Body body = ShenyuRequest.Body.empty();

  public ShenyuRequest request() {
    return ShenyuRequest.create(this.httpMethod, this.url + this.path, this.headers, this.body, this);
  }

  public String getUrl() {
    return url;
  }

  public RequestTemplate setUrl(String url) {
    this.url = url;
    return this;
  }

  public ShenyuRequest.HttpMethod getHttpMethod() {
    return httpMethod;
  }

  public RequestTemplate setHttpMethod(ShenyuRequest.HttpMethod method) {
    this.httpMethod = method;
    return this;
  }

  public Map<String, Collection<String>> getHeaders() {
    return headers;
  }

  public RequestTemplate setHeaders(Map<String, Collection<String>> headers) {
    this.headers = headers;
    return this;
  }

  public ShenyuRequest.Body getBody() {
    return body;
  }

  public RequestTemplate setBody(ShenyuRequest.Body body) {
    this.body = body;
    return this;
  }

  public String getPath() {
    return path;
  }

  public RequestTemplate setPath(String path) {
    this.path = path;
    return this;
  }

  public List<ParamMetadata> getParamMetadataList() {
    return paramMetadataList;
  }

  public RequestTemplate setParamMetadataList(List<ParamMetadata> paramMetadataList) {
    this.paramMetadataList = paramMetadataList;
    return this;
  }

  public static class ParamMetadata {

    /**
     * paramAnnotations.
     */
    private Annotation[] paramAnnotations;

    /**
     * paramType.
     */
    private Class<?> paramType;

    /**
     * isPrimitive.
     */
    private boolean isPrimitive;

    /**
     * paramIndexOnMethod.
     */
    private int paramIndexOnMethod;

    public Annotation[] getParamAnnotations() {
      return paramAnnotations;
    }

    public void setParamAnnotations(Annotation[] paramAnnotations) {
      this.paramAnnotations = paramAnnotations;
    }

    public Class<?> getParamType() {
      return paramType;
    }

    public void setParamType(Class<?> paramType) {
      this.paramType = paramType;
    }

    public boolean isPrimitive() {
      return isPrimitive;
    }

    public void setPrimitive(boolean primitive) {
      isPrimitive = primitive;
    }

    public int getParamIndexOnMethod() {
      return paramIndexOnMethod;
    }

    public void setParamIndexOnMethod(int paramIndexOnMethod) {
      this.paramIndexOnMethod = paramIndexOnMethod;
    }

  }
}
