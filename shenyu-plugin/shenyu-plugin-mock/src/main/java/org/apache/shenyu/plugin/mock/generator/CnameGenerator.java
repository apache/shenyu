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

import java.util.Random;

/**
 * Chinese name generator.
 */
public class CnameGenerator extends AbstractGenerator<String> {
    
    private static final String[] FIRST_NAME = {"李", "王", "张", "刘", "陈", "杨", "赵", "黄", "周", "吴",
        "徐", "孙", "胡", "朱", "高", "林", "何", "郭", "马", "罗", "梁", "宋", "郑", "谢", "韩", "唐",
        "冯", "于", "董", "萧", "程", "曹", "袁", "邓", "许", "傅", "沈", "曾", "彭", "吕", "苏", "卢",
        "蒋", "蔡", "贾", "丁", "魏", "薛", "叶", "阎", "余", "潘", "杜", "戴", "夏", "钟", "汪", "田",
        "任", "姜", "范", "方", "石", "姚", "谭", "廖", "邹", "熊", "金", "陆", "郝", "孔", "白", "崔",
        "康", "毛", "邱", "秦", "江", "史", "顾", "侯", "邵", "孟", "龙", "万", "段", "漕", "钱", "汤",
        "尹", "黎", "易", "常", "武", "乔", "贺", "赖", "龚", "文", "诸葛", "欧阳", "上官"};
    
    private static final String[] LAST_NAME = {"骏", "宇", "玄", "璀", "紫", "子", "全", "超", "益", "莉", "信",
        "美", "奎", "琪", "豪", "浩", "槐", "文", "巧", "治", "瑜", "雯", "诗", "涵", "曦", "嵘", "天",
        "誉", "喜", "伟", "嘉", "欣", "卿", "钰", "勇", "寅", "天", "宸", "兵", "祥", "运", "昊", "泽",
        "仁", "淳", "轩", "子", "泳", "瑶", "源", "杰", "正", "驿", "豪", "财", "熙", "海", "辉", "辉",
        "天", "华", "峻", "龙", "嘉", "诚", "嘉", "运", "财", "喜", "伟", "天", "俊", "熙", "熙", "利",
        "君", "帝", "云", "海", "伟", "熙", "霞", "艳", "宗", "书", "文", "国", "熙", "林", "天", "录",
        "熙", "明", "俊", "敬", "贵", "艺", "心", "艾", "悦", "思", "甜", "帅", "梅", "莲", "婷", "伯",
        "沁", "苹", "苗", "龙", "尔", "仕", "墨", "驿", "森", "桥", "丹", "东", "墨", "满", "阳", "骏",
        "鹏", "毓", "昆", "金", "宇", "宇", "木", "金", "豪", "天", "洋", "宇", "捷", "治", "智", "浩",
        "泽", "搏", "朗", "朗", "桐", "喜", "伊", "萱", "彤", "正", "裕", "馨", "湘", "锦", "湘", "玉",
        "元", "坤", "子", "侦", "山", "水", "茂", "阳", "海", "乐", "晨", "杰", "高", "启", "夫", "玉",
        "舟", "凌", "璇", "雨", "扬", "晨", "亭", "艳", "昊", "洋", "晴", "雨", "雨", "晨", "雨", "辰",
        "骞", "娴", "颖", "晞", "朝", "晓", "小", "蕾", "雯", "悦", "月", "秋", "绮", "书", "彤", "诗",
        "韬", "羿", "程", "惠", "忻"};
    
    @Override
    public String getName() {
        return "cname";
    }
    
    @Override
    public String generate() {
        Random random = new Random();
        return FIRST_NAME[random.nextInt(FIRST_NAME.length)]
            + LAST_NAME[random.nextInt(LAST_NAME.length)]
            + ((random.nextInt(2) == 1) ? LAST_NAME[random.nextInt(LAST_NAME.length)] : "");
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
        return rule.matches("^cname$");
    }
    
}
