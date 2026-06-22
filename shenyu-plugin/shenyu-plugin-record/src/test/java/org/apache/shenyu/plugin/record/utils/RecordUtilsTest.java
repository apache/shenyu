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

package org.apache.shenyu.plugin.record.utils;

import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class RecordUtilsTest {

    @Test
    public void testIsNotBinaryTypeWithJson() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.APPLICATION_JSON);
        assertTrue(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithPlainText() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.TEXT_PLAIN);
        assertTrue(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithHtml() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.TEXT_HTML);
        assertTrue(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithImage() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.IMAGE_PNG);
        assertFalse(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithOctetStream() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.APPLICATION_OCTET_STREAM);
        assertFalse(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithPdf() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.APPLICATION_PDF);
        assertFalse(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithMultipart() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.MULTIPART_FORM_DATA);
        assertFalse(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithJavascript() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.valueOf("application/javascript"));
        assertFalse(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithCss() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(org.springframework.http.MediaType.valueOf("text/css"));
        assertFalse(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testIsNotBinaryTypeWithNullHeaders() {
        assertTrue(RecordUtils.isNotBinaryType(null));
    }

    @Test
    public void testIsNotBinaryTypeWithNoContentType() {
        HttpHeaders headers = new HttpHeaders();
        assertTrue(RecordUtils.isNotBinaryType(headers));
    }

    @Test
    public void testGetHeadersWithSingleValue() {
        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Type", "application/json");
        Map<String, String> result = RecordUtils.getHeaders(headers);
        assertEquals("application/json", result.get("Content-Type"));
    }

    @Test
    public void testGetHeadersWithMultipleValues() {
        HttpHeaders headers = new HttpHeaders();
        headers.add("Accept", "application/json");
        headers.add("Accept", "text/html");
        Map<String, String> result = RecordUtils.getHeaders(headers);
        assertEquals("application/json,text/html", result.get("Accept"));
    }

    @Test
    public void testGetHeadersWithNullHeaders() {
        Map<String, String> result = RecordUtils.getHeaders(null);
        assertTrue(result.isEmpty());
    }

    @Test
    public void testGetHeadersWithEmptyHeaders() {
        HttpHeaders headers = new HttpHeaders();
        Map<String, String> result = RecordUtils.getHeaders(headers);
        assertTrue(result.isEmpty());
    }
}