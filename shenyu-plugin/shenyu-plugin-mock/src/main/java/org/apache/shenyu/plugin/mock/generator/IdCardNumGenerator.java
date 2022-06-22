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

package org.apache.shenyu.plugin.mock.generator;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import org.apache.shenyu.plugin.mock.util.RandomUtil;

/**
 * 18-digit ID number generator.
 */
public class IdCardNumGenerator extends AbstractGenerator<String> {
    
    @Override
    public String getName() {
        return "idcard";
    }
    
    @Override
    public String generate() {
        String[] provinces = {"11", "12", "13", "14", "15", "21", "22", "23",
            "31", "32", "33", "34", "35", "36", "37", "41", "42", "43",
            "44", "45", "46", "50", "51", "52", "53", "54", "61", "62",
            "63", "64", "65", "71", "81", "82"};
        String province = provinces[RandomUtil.randomInt(0, provinces.length - 1)];
        String[] cities = {"01", "02", "03", "04", "05", "06", "07", "08",
            "09", "10", "21", "22", "23", "24", "25", "26", "27", "28"};
        String city = cities[RandomUtil.randomInt(0, cities.length - 1)];
        
        String[] countries = {"01", "02", "03", "04", "05", "06", "07", "08",
            "09", "10", "21", "22", "23", "24", "25", "26", "27", "28",
            "29", "30", "31", "32", "33", "34", "35", "36", "37", "38"};
        String county = countries[RandomUtil.randomInt(0, countries.length - 1)];
        SimpleDateFormat dft = new SimpleDateFormat("yyyyMMdd");
        Date beginDate = new Date();
        Calendar date = Calendar.getInstance();
        date.setTime(beginDate);
        date.set(Calendar.DATE,
            date.get(Calendar.DATE) - RandomUtil.randomInt(0, 365 * 100));
        String birth = dft.format(date.getTime());
        String no = RandomUtil.randomInt(0, 999) + "";
        String[] checks = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
            "X"};
        String check = checks[RandomUtil.randomInt(0, checks.length - 1)];
        return province + city + county + birth + no + check;
    }
    
    @Override
    public int getParamSize() {
        return 0;
    }
    
    @Override
    void initParam() {
    }
    
    @Override
    public boolean match(final String rule) {
        return rule.matches("^idcard$");
    }
}
