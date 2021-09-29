#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

import os
import shutil
from datetime import datetime

uncompress_directory = '.' + os.sep + '.tmp'
package_names = []
directory_names = []
source_directory_full_name = ''
directory_full_names = []
potential_binary_suffix = ['.zip', '.tar.gz', '.gzip', '.7z', '.class', '.jar', '.war']


def check_size_and_uncompress_packages():
    global package_names
    shutil.rmtree(uncompress_directory, ignore_errors=True)
    names = os.listdir('.')
    for name in names:
        if name.rfind('.zip') != -1:
            package_names.append(name)
            shutil.unpack_archive(name, uncompress_directory)
            check_size(name, 10000000)
        if name.rfind('.tar.gz') != -1:
            package_names.append(name)
            shutil.unpack_archive(name, uncompress_directory)
            check_size(name, 200000000)


def check_size(name, size):
    if os.path.getsize(name) > size:
        print('ERROR :: ' + name + ' File size > ' + str(size / 1000000) + 'MB')
    # print(name, str(os.path.getsize(name)) + ' Byte')


def check_common():
    global directory_names
    global directory_full_names
    global source_directory_full_name
    directory_names = os.listdir(uncompress_directory)
    for name in directory_names + package_names:
        check_name(name)
    for name in directory_names:
        directory_full_name = uncompress_directory + os.sep + name + os.sep
        directory_full_names.append(directory_full_name)
        if name.rfind('-src') != -1:
            source_directory_full_name = directory_full_name
        check_required_files(directory_full_name + 'LICENSE', directory_full_name + 'NOTICE', directory_full_name + 'DISCLAIMER')
        check_year_in_notice(directory_full_name + 'NOTICE')


def check_name(name):
    if name.find('incubating') == -1:
        print('ERROR :: ' + name + ' does not contains incubating word')


def check_required_files(*names):
    for name in names:
        if not os.path.isfile(name):
            print('ERROR :: ' + name + ' does not exists')


def check_year_in_notice(file_name):
    year = str(datetime.now().year)
    with open(file_name) as file_obj:
        if file_obj.readlines()[1].find(year) == -1:
            print('ERROR :: Year in the NOTICE is not ' + year)


def check_all_text_files():
    all_file_names = []
    for root, dirs, files in os.walk(source_directory_full_name):
        if len(files) != 0:
            all_file_names += files
        if len(files) == 0 and len(dirs) == 0:
            print('ERROR :: Empty directory detected ' + root)

    # print(len(all_file_names))
    for name in all_file_names:
        for suffix in potential_binary_suffix:
            if name.rfind(suffix) != -1:
                print('ERROR :: ' + name + ' is not allowed here')


if __name__ == '__main__':
    check_size_and_uncompress_packages()
    check_common()
    check_all_text_files()

