#!/bin/bash

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