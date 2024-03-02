/*
 * Copyright 1999-2018 Alibaba Group Holding Ltd.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * ZipUtil.
 */
public final class ZipUtil {

    private static final Logger LOG = LoggerFactory.getLogger(ZipUtil.class);

    private ZipUtil() {
    }

    public static class ZipItem {

        private String itemName;

        private String itemData;

        public ZipItem(String itemName, String itemData) {
            this.itemName = itemName;
            this.itemData = itemData;
        }

        public String getItemName() {
            return itemName;
        }

        public void setItemName(String itemName) {
            this.itemName = itemName;
        }

        public String getItemData() {
            return itemData;
        }

        public void setItemData(String itemData) {
            this.itemData = itemData;
        }
    }

    public static class UnZipResult {

        private List<ZipItem> zipItemList;

        public UnZipResult(List<ZipItem> zipItemList) {
            this.zipItemList = zipItemList;
        }

        public List<ZipItem> getZipItemList() {
            return zipItemList;
        }

        public void setZipItemList(List<ZipItem> zipItemList) {
            this.zipItemList = zipItemList;
        }

    }

    /**
     * zip method.
     */
    public static byte[] zip(List<ZipItem> source) {
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
     * unzip method.
     */
    public static UnZipResult unzip(byte[] source) {
        List<ZipItem> itemList = Lists.newArrayList();
        try (ZipInputStream zipIn = new ZipInputStream(new ByteArrayInputStream(source))) {
            ZipEntry entry;
            while ((entry = zipIn.getNextEntry()) != null) {
                if (entry.isDirectory()) {
                    continue;
                }
                try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                    byte[] buffer = new byte[1024];
                    int offset;
                    while ((offset = zipIn.read(buffer)) != -1) {
                        out.write(buffer, 0, offset);
                    }
                    String entryName = entry.getName();
                    itemList.add(new ZipItem(entryName, out.toString("UTF-8")));
                } catch (IOException e) {
                    LOG.error("unzip error", e);
                }
            }
        } catch (IOException e) {
            LOG.error("unzip error", e);
        }
        return new UnZipResult(itemList);
    }

}
