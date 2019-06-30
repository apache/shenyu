/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.common.utils;

import java.security.MessageDigest;

/**
 * @author huangxiaofeng
 * @date 2019/6/29 22:45
 */
public abstract class MD5Utils {

	static public String md5(String src, String charset){
		MessageDigest md5 = null;
		StringBuilder hexValue = new StringBuilder(32);
		try{
			md5 = MessageDigest.getInstance("MD5");

			byte[] byteArray = null;
			byteArray = src.getBytes(charset);

			byte[] md5Bytes = md5.digest(byteArray);

			for (int i = 0; i < md5Bytes.length; i++) {
				int val = ((int) md5Bytes[i]) & 0xff;
				if (val < 16) {
					hexValue.append("0");
				}
				hexValue.append(Integer.toHexString(val));
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return hexValue.toString();
	}

	static public String md5(String src){
		return md5(src, "UTF-8");
	}

}
