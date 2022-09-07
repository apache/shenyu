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
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * An immutable request to an http server.
 */
public final class ShenyuRequest implements Serializable {

  public enum HttpMethod {
    GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH
  }

  /**
   * Builds a Request. All parameters must be effectively immutable, via safe copies.
   *
   * @param httpMethod for the request.
   * @param url for the request.
   * @param headers to include.
   * @param body of the request, can be {@literal null}
   * @return a Request
   */
  public static ShenyuRequest create(HttpMethod httpMethod,
                               String url,
                               Map<String, Collection<String>> headers,
                               Body body,
                               RequestTemplate requestTemplate) {
    return new ShenyuRequest(httpMethod, url, headers, body, requestTemplate);
  }

  private final HttpMethod httpMethod;
  private final String url;
  private final Map<String, Collection<String>> headers;
  private final Body body;
  private final RequestTemplate requestTemplate;

  /**
   * Creates a new Request.
   *
   * @param method of the request.
   * @param url for the request.
   * @param headers for the request.
   * @param body for the request, optional.
   * @param requestTemplate used to build the request.
   */
  ShenyuRequest(HttpMethod method,
                String url,
                Map<String, Collection<String>> headers,
                Body body,
                RequestTemplate requestTemplate) {
    this.httpMethod = checkNotNull(method, "httpMethod of %s", method.name());
    this.url = checkNotNull(url, "url");
    this.headers = checkNotNull(headers, "headers of %s %s", method, url);
    this.body = body;
    this.requestTemplate = requestTemplate;
  }

  /**
   * Http Method for this request.
   *
   * @return the HttpMethod string
   * @deprecated @see {@link #httpMethod()}
   */
  @Deprecated
  public String method() {
    return httpMethod.name();
  }

  /**
   * Http Method for the request.
   *
   * @return the HttpMethod.
   */
  public HttpMethod httpMethod() {
    return this.httpMethod;
  }


  /**
   * URL for the request.
   *
   * @return URL as a String.
   */
  public String url() {
    return url;
  }

  /**
   * Request Headers.
   *
   * @return the request headers.
   */
  public Map<String, Collection<String>> headers() {
    return Collections.unmodifiableMap(headers);
  }

  /**
   * Charset of the request.
   *
   * @return the current character set for the request, may be {@literal null} for binary data.
   */
  public Charset charset() {
    return body.encoding;
  }

  /**
   * If present, this is the replayable body to send to the server. In some cases, this may be
   * interpretable as text.
   *
   * @see #charset()
   */
  public byte[] body() {
    return body.data;
  }

  public boolean isBinary() {
    return body.isBinary();
  }

  /**
   * Request Length.
   *
   * @return size of the request body.
   */
  public int length() {
    return this.body.length();
  }

  /**
   * Request as an HTTP/1.1 request.
   *
   * @return the request.
   */
  @Override
  public String toString() {
    final StringBuilder builder = new StringBuilder();
    builder.append(httpMethod).append(' ').append(url).append(" HTTP/1.1\n");
    for (final String field : headers.keySet()) {
      Collection<String> values = Optional.ofNullable(headers.get(field)).orElse(Collections.emptyList());
      for (final String value : values) {
        builder.append(field).append(": ").append(value).append('\n');
      }
    }
    if (body != null) {
      builder.append('\n').append(body.asString());
    }
    return builder.toString();
  }


  public RequestTemplate requestTemplate() {
    return this.requestTemplate;
  }

  /**
   * Request Body
   * <p>
   * Considered experimental, will most likely be made internal going forward.
   * </p>
   */
  public static class Body implements Serializable {

    private transient Charset encoding;

    private byte[] data;

    private Body() {
      super();
    }

    private Body(byte[] data) {
      this.data = data;
    }

    private Body(byte[] data, Charset encoding) {
      this.data = data;
      this.encoding = encoding;
    }

    public Optional<Charset> getEncoding() {
      return Optional.ofNullable(this.encoding);
    }

    public int length() {
      /* calculate the content length based on the data provided */
      return data != null ? data.length : 0;
    }

    public byte[] asBytes() {
      return data;
    }

    public String asString() {
      return !isBinary()
          ? new String(data, encoding)
          : "Binary data";
    }

    public boolean isBinary() {
      return encoding == null || data == null;
    }

    public static Body create(String data) {
      return new Body(data.getBytes());
    }

    public static Body create(String data, Charset charset) {
      return new Body(data.getBytes(charset), charset);
    }

    public static Body create(byte[] data) {
      return new Body(data);
    }

    public static Body create(byte[] data, Charset charset) {
      return new Body(data, charset);
    }

    public static Body empty() {
      return new Body();
    }

  }
}
