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

package org.dromara.soul.web.logo;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.utils.VersionUtils;
import org.junit.Test;

/**
 * The TestCase for SoulLogo.
 *
 * @author itmiwang
 */
@Slf4j
public final class SoulLogoTest {
    
    private static final String SOUL_LOGO = "\n"
            + "                 _  \n"
            + "                | | \n"
            + " ___  ___  _   _| | \n"
            + "/ __|/ _ \\| | | | |\n"
            + "\\__ \\ (_) | |_| | |\n"
            + "|___/\\___/ \\__,_|_|\n"
            + "                    \n"
            + "                   \n";
    
    @Test
    public void buildBannerText() {
        String buildBannerText = Constants.LINE_SEPARATOR
                + Constants.LINE_SEPARATOR
                + SOUL_LOGO
                + Constants.LINE_SEPARATOR
                + " :: Soul :: (v" + VersionUtils.getVersion(getClass(), "2.0.2") + ")"
                + Constants.LINE_SEPARATOR;
        log.info(buildBannerText);
    }
}
