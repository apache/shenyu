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

package org.apache.shenyu.plugin.logging.common.datamask;

import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
public class DataMaskByCharReplace implements DataMaskInterface {

    private static final Character MASK = '*';

    @Override
    public String mask(final String data) {

        if (!StringUtils.hasLength(data)) {
            return "";
        }
        return doMask(data, data.length() / 2);
    }

    private String doMask(final String data, final int maskNum) {

        if(data.length()==1){
            return "*";
        }
        StringBuilder sb = new StringBuilder(data);
        int mid = data.length() / 2;
        int l = mid - 1;
        int r = mid;
        int currentMaskNum = 0;
        while (currentMaskNum < maskNum) {
            int tempMaskNum = currentMaskNum;
            if (r < data.length()) {
                sb.setCharAt(r, MASK);
                r++;
                currentMaskNum++;
            }
            if (currentMaskNum >= maskNum) {
                break;
            }
            if (l >= 0) {
                sb.setCharAt(l, MASK);
                l--;
                currentMaskNum++;
            }
            if (tempMaskNum == currentMaskNum) {
                break;
            }
        }
        return sb.toString();
    }
}
