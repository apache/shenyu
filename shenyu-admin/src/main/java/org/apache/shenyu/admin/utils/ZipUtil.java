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

package org.apache.shenyu.admin.utils;

import com.google.common.collect.Lists;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * ZipUtil.
 */
public final class ZipUtil {

    private static final Logger LOG = LoggerFactory.getLogger(ZipUtil.class);

    /**
     * 100 MB per entry.
     */
    private static final long MAX_ENTRY_SIZE = 100L * 1024 * 1024;

    /**
     * 200 MB total.
     */
    private static final long MAX_TOTAL_SIZE = 200L * 1024 * 1024;

    private static final int MAX_ENTRY_COUNT = 1000;

    private ZipUtil() {
    }

    /**
     * zip method.
     *
     * @param source zip source
     * @return byte array
     */
    public static byte[] zip(final List<ZipItem> source) {
        byte[] result = null;
        try (ByteArrayOutputStream byteOut = new ByteArrayOutputStream(); ZipOutputStream zipOut = new ZipOutputStream(
                byteOut)) {
            for (ZipItem item : source) {
                zipOut.putNextEntry(new ZipEntry(item.getItemName()));
                zipOut.write(item.getItemData().getBytes(StandardCharsets.UTF_8));
            }
            zipOut.flush();
            zipOut.finish();
            result = byteOut.toByteArray();
        } catch (IOException e) {
            LOG.error("an error occurred while compressing data.", e);
        }
        return result;
    }

    /**
     * unzip method with default size limits.
     *
     * @param source source
     * @return unzip result
     */
    public static UnZipResult unzip(final byte[] source) {
        return unzip(source, MAX_ENTRY_SIZE, MAX_TOTAL_SIZE, MAX_ENTRY_COUNT);
    }

    /**
     * unzip method with configurable size limits.
     *
     * @param source source
     * @param maxEntrySize max entry size
     * @param maxTotalSize max total size
     * @param maxEntryCount max entry count
     * @return unzip result
     */
    public static UnZipResult unzip(final byte[] source, final long maxEntrySize,
                                    final long maxTotalSize, final int maxEntryCount) {
        List<ZipItem> itemList = Lists.newArrayList();
        long totalSize = 0L;
        int entryCount = 0;
        try (ZipInputStream zipIn = new ZipInputStream(new ByteArrayInputStream(source))) {
            ZipEntry entry;
            while (Objects.nonNull(entry = zipIn.getNextEntry())) {
                if (entry.isDirectory()) {
                    continue;
                }
                entryCount++;
                if (entryCount > maxEntryCount) {
                    throw new IllegalArgumentException("entry count exceeds maximum of " + maxEntryCount);
                }
                try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                    byte[] buffer = new byte[1024];
                    int offset;
                    long entrySize = 0L;
                    while ((offset = zipIn.read(buffer)) != -1) {
                        entrySize += offset;
                        totalSize += offset;
                        if (entrySize > maxEntrySize) {
                            throw new IllegalArgumentException("entry size exceeds maximum allowed value.");
                        }
                        if (totalSize > maxTotalSize) {
                            throw new IllegalArgumentException("total size exceeds maximum allowed value.");
                        }
                        out.write(buffer, 0, offset);
                    }
                    String entryName = entry.getName();
                    itemList.add(new ZipItem(entryName, out.toString(StandardCharsets.UTF_8)));
                } catch (IOException e) {
                    LOG.error("unzip error", e);
                }
            }
        } catch (IOException e) {
            LOG.error("unzip error", e);
        }
        return new UnZipResult(itemList);
    }

    public static class ZipItem {

        private final String itemName;

        private final String itemData;

        public ZipItem(final String itemName, final String itemData) {
            this.itemName = itemName;
            this.itemData = itemData;
        }

        /**
         * Gets the item name.
         *
         * @return the item name
         */
        public String getItemName() {
            return itemName;
        }

        /**
         * Gets the item data.
         *
         * @return the item data
         */
        public String getItemData() {
            return itemData;
        }

    }

    public static class UnZipResult {
        /**
         * zip item list.
         */
        private final List<ZipItem> zipItemList;

        public UnZipResult(final List<ZipItem> zipItemList) {
            this.zipItemList = zipItemList;
        }

        /**
         * Gets the zip item list.
         *
         * @return the zip item list
         */
        public List<ZipItem> getZipItemList() {
            return zipItemList;
        }

    }

}
