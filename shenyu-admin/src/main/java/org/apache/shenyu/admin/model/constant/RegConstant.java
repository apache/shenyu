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

package org.apache.shenyu.admin.model.constant;

/**
 * RegConstant.
 */
public final class RegConstant {

    /**
     * Minimum length of 8, including upper and lower case letters, numbers and special characters.
     */
    public static final String PASSWORD_RULE = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)(?=.*[@$!%*?&#.=_+-])[A-Za-z\\d@$!%*?&#.=_+-]{8,}$";


    /**
     * At least 8 in length, containing at least one letter and one number.
     */
    public static final String PASSWORD_RULE_L0 = "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$";

    /**
     * Minimum length of 8, containing at least one letter and one number and one special character.
     */
    public static final String PASSWORD_RULE_L1 = "^(?=.*[A-Za-z])(?=.*\\d)(?=.*[@$!%*?&#.=_+-])[A-Za-z\\d@$!%*?&#.=_+-]{8,}$";

    /**
     * At least 8 in length, with at least one number and both upper and lower case letters.
     */
    public static final String PASSWORD_RULE_L2 = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)[a-zA-Z\\d]{8,}$";

    /**
     * Minimum length of 8 - 16, including upper and lower case letters, numbers and special characters.
     */
    public static final String PASSWORD_RULE_L3 = "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\)(?=.*[@$!%*?&#.=_+-])[A-Za-z\\d@$!%*?&#.=_+-]{8,16}$";

    private RegConstant() {

    }

}
