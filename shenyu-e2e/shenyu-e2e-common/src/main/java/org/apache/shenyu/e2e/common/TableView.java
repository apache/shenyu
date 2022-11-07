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

package org.apache.shenyu.e2e.common;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class TableView {
    private List<String[]> table = new ArrayList<>();
    
    private final int sizeOfColumnsInRow;
    private final String[] headers;
    
    private int[] maxContentLengthOfColumns;
    
    public TableView(String... headers) {
        this.headers = headers;
        this.sizeOfColumnsInRow = headers.length;
        this.maxContentLengthOfColumns = new int[headers.length];
        for (int i = 0; i < headers.length; i++) {
            maxContentLengthOfColumns[i] = Math.max(maxContentLengthOfColumns[i], headers[i].length());
        }
    }
    
    public TableView addRow(Object... row) {
        if (row.length > sizeOfColumnsInRow) {
            throw new IllegalArgumentException(String.format("Expecting the size of row to be %d but was %s", sizeOfColumnsInRow, row.length));
        }
        String[] columns = Arrays.stream(row).map(Object::toString).map(String::trim).toArray(String[]::new);
        for (int i = 0; i < columns.length; i++) {
            maxContentLengthOfColumns[i] = Math.max(maxContentLengthOfColumns[i], columns[i].length());
        }
        table.add(columns);
        return this;
    }
    
    public String printAsString() {
        String[] template = new String[maxContentLengthOfColumns.length];
        
        StringBuilder separator = new StringBuilder("+");
        for (int i = 0; i < maxContentLengthOfColumns.length; i++) {
            int length = maxContentLengthOfColumns[i];
            for (int j = -2; j < length; j++) {
                separator.append('-');
            }
            separator.append("+");
            template[i] = String.format(" %%-%ds ", length);
        }
        separator.append(System.lineSeparator());
        
        StringBuilder tableBuilder = new StringBuilder();
        tableBuilder.append(printHeaders(headers, template, separator));
        
        for (String[] row : table) {
            tableBuilder.append(printRow(row, template, separator));
        }
        
        return tableBuilder.toString().trim();
    }
    
    private StringBuilder printHeaders(String[] headers, String[] template, StringBuilder separator) {
        StringBuilder builder = new StringBuilder(separator);
        for (int i = 0; i < headers.length; i++) {
            builder.append("|").append(String.format(template[i], headers[i]));
        }
        builder.append("|").append(System.lineSeparator()).append(separator);
        return builder;
    }
    
    private StringBuilder printRow(String[] columns, String[] template, StringBuilder separator) {
        StringBuilder rowBuilder = new StringBuilder();
        for (int i = 0; i < columns.length; i++) {
            rowBuilder.append("|").append(String.format(template[i], columns[i]));
        }
        for (int i = columns.length; i < template.length; i++) {
            rowBuilder.append("|").append(String.format(template[i], ""));
        }
        rowBuilder.append("|").append(System.lineSeparator()).append(separator);
        return rowBuilder;
    }
    
}
