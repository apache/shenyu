#!/bin/bash

# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

echo 'Remove duplicated jar files...'

# 获取第一个目录下的所有文件
dir1=$1
array1=($(find $dir1 -type f))

# 获取第二个目录下的所有文件
dir2=$2
array2=($(find $dir2 -type f))

# 遍历两个数组，检查是否存在重复的文件
for file in "${array1[@]}"; do
    for otherfile in "${array2[@]}"; do
        if [[ $(basename "$file") == $(basename "$otherfile") ]]; then
            # 删除重复的文件
            rm -f "$otherfile"
            break;
        fi
    done
done