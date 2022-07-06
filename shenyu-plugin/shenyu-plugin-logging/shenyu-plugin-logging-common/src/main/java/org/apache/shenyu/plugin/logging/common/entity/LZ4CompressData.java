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

package org.apache.shenyu.plugin.logging.common.entity;

/**
 * Lz4 compressed data.
 */
public class LZ4CompressData {

    private int length;

    private byte[] compressedData;

    public LZ4CompressData(final int length, final byte[] compressedData) {
        this.length = length;
        this.compressedData = compressedData;
    }

    /**
     * get the exact size of the original input.
     *
     * @return the exact size of the original input
     */
    public int getLength() {
        return length;
    }

    /**
     * set the exact size of the original input.
     *
     * @param length the exact size of the original input.
     */
    public void setLength(final int length) {
        this.length = length;
    }

    /**
     * get the compressed data.
     *
     * @return the compressed data
     */
    public byte[] getCompressedData() {
        return compressedData;
    }

    /**
     * set the compressed data.
     *
     * @param compressedData the compressed data
     */
    public void setCompressedData(final byte[] compressedData) {
        this.compressedData = compressedData;
    }
}
