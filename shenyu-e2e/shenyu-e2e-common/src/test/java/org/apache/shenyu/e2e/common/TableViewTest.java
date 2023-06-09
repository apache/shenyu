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

import lombok.AllArgsConstructor;
import lombok.ToString;
import org.assertj.core.api.Assertions;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;

import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TableViewTest {
    
    @Test
    void testTableViewPrint() {
        TableView tableView = new TableView("header 1", "header 2", "header 3", "header 4");
        tableView.addRow("column 1", "column 2", "column 3", "column 4");
        tableView.addRow(1, new Cell("123"), 1d, Lists.newArrayList("a", "b"));
        
        Assertions.assertThat(tableView.printAsString())
                .isEqualTo(Stream.<String>builder()
                        .add("+----------+---------------------------------+----------+----------+")
                        .add("| header 1 | header 2                        | header 3 | header 4 |")
                        .add("+----------+---------------------------------+----------+----------+")
                        .add("| column 1 | column 2                        | column 3 | column 4 |")
                        .add("+----------+---------------------------------+----------+----------+")
                        .add("| 1        | TableViewTest.Cell(content=123) | 1.0      | [a, b]   |")
                        .add("+----------+---------------------------------+----------+----------+")
                        .build()
                        .collect(Collectors.joining(System.lineSeparator()))
                );
    }
    
    @Test
    void testUnexpectedColumns() {
        TableView tableView = new TableView("header 1", "header 2", "header 3", "header 4");
        Assertions.assertThatExceptionOfType(IllegalArgumentException.class)
                .isThrownBy(() -> tableView.addRow("column 1", "column 2", "column 3", "column 4", "column 5"))
                .withMessage("Expecting the size of row to be 4 but was 5");
    }
    
    @Test
    void testMissingCell() {
        TableView tableView = new TableView("h1", "h2");
        tableView.addRow("c1");
        
        Assertions.assertThat(tableView.printAsString()).isEqualTo(
                Stream.<String>builder()
                        .add("+----+----+")
                        .add("| h1 | h2 |")
                        .add("+----+----+")
                        .add("| c1 |    |")
                        .add("+----+----+")
                        .build()
                        .collect(Collectors.joining(System.lineSeparator()))
        );
    }
    
    @ToString
    @AllArgsConstructor
    static class Cell {
        private final String content;
    }
}
