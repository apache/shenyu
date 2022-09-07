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

package org.apache.shenyu.sdk.core;


import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Request Builder for an HTTP.
 */
public final class RequestTemplate implements Serializable {

  private String url;

  private String path;

  private ShenyuRequest.HttpMethod httpMethod;

  private List<ParamMetadata> paramMetadataList;

  private Map<String, Collection<String>> headers = new HashMap<>();

  private String body;

  /**
   * request.
   *
   * @return {@link ShenyuRequest}
   */
  public ShenyuRequest request() {
    return ShenyuRequest.create(this.httpMethod, this.url + this.path, this.headers, this.body, this);
  }

  /**
   * getUrl.
   *
   * @return {@link String}
   */
  public String getUrl() {
    return url;
  }

  /**
   * setUrl.
   *
   * @param url url
   * @return {@link RequestTemplate}
   */
  public RequestTemplate setUrl(final String url) {
    this.url = url;
    return this;
  }

  /**
   * getHttpMethod.
   *
   * @return {@link ShenyuRequest.HttpMethod}
   */
  public ShenyuRequest.HttpMethod getHttpMethod() {
    return httpMethod;
  }

  /**
   * setHttpMethod.
   *
   * @param method method
   * @return {@link RequestTemplate}
   */
  public RequestTemplate setHttpMethod(final ShenyuRequest.HttpMethod method) {
    this.httpMethod = method;
    return this;
  }

  /**
   * getHeaders.
   *
   * @return {@link Map}
   */
  public Map<String, Collection<String>> getHeaders() {
    return headers;
  }

  /**
   * setHeaders.
   *
   * @param headers headers
   * @return {@link RequestTemplate}
   */
  public RequestTemplate setHeaders(final Map<String, Collection<String>> headers) {
    this.headers = headers;
    return this;
  }

  /**
   * getBody.
   *
   * @return {@link String}
   */
  public String getBody() {
    return body;
  }

  /**
   * setBody.
   *
   * @param body body
   */
  public void setBody(String body) {
    this.body = body;
  }

  /**
   * getPath.
   *
   * @return {@link String}
   */
  public String getPath() {
    return path;
  }

  /**
   * setPath.
   *
   * @param path path
   * @return {@link RequestTemplate}
   */
  public RequestTemplate setPath(final String path) {
    this.path = path;
    return this;
  }

  /**
   * getParamMetadataList.
   *
   * @return {@link List}
   */
  public List<ParamMetadata> getParamMetadataList() {
    return paramMetadataList;
  }

  /**
   * setParamMetadataList.
   *
   * @param paramMetadataList paramMetadataList
   * @return {@link RequestTemplate}
   */
  public RequestTemplate setParamMetadataList(final List<ParamMetadata> paramMetadataList) {
    this.paramMetadataList = paramMetadataList;
    return this;
  }

  /**
   * method ParamMetadata.
   */
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

    /**
     * getParamAnnotations.
     *
     * @return {@link Annotation[]}
     */
    public Annotation[] getParamAnnotations() {
      return paramAnnotations;
    }

    /**
     * setParamAnnotations.
     *
     * @param paramAnnotations paramAnnotations
     */
    public void setParamAnnotations(final Annotation[] paramAnnotations) {
      this.paramAnnotations = paramAnnotations;
    }

    /**
     * getParamType.
     *
     * @return {@link Class<?>}
     */
    public Class<?> getParamType() {
      return paramType;
    }

    /**
     * setParamType.
     *
     * @param paramType paramType
     */
    public void setParamType(final Class<?> paramType) {
      this.paramType = paramType;
    }

    /**
     * isPrimitive.
     *
     * @return {@link boolean}
     */
    public boolean isPrimitive() {
      return isPrimitive;
    }

    /**
     * setPrimitive.
     *
     * @param primitive primitive
     */
    public void setPrimitive(final boolean primitive) {
      isPrimitive = primitive;
    }

    /**
     * getParamIndexOnMethod.
     *
     * @return {@link int}
     */
    public int getParamIndexOnMethod() {
      return paramIndexOnMethod;
    }

    /**
     * setParamIndexOnMethod.
     *
     * @param paramIndexOnMethod paramIndexOnMethod
     */
    public void setParamIndexOnMethod(final int paramIndexOnMethod) {
      this.paramIndexOnMethod = paramIndexOnMethod;
    }

  }
}
