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

package org.apache.shenyu.admin.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.PreparedStatement;
import java.sql.SQLException;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Test case for {@link BaseTrigger}.
 */
public class BaseTriggerTest {

    @Test
    public void sqlExecute() {
        final Object[] newRow = new Object[4];
        newRow[0] = null;
        newRow[1] = new Object();
        newRow[2] = new Object();
        newRow[3] = new Object();
        Assertions.assertDoesNotThrow(() -> BaseTrigger.sqlExecute(newRow, mock(PreparedStatement.class)));
    }

    @Test
    public void testSqlExecuteWithNullFirstElement() throws SQLException {
        // Test case where newRow[0] is null - should execute SQL
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {null, "value1", "value2", "value3", "value4"};

        BaseTrigger.sqlExecute(newRow, mockStatement);

        // Verify that setObject was called for the UUID at position 1
        verify(mockStatement).setObject(eq(1), any(String.class));
        // Verify that setObject was called for each element in the loop (i=1 to length-3)
        // newRow.length = 5, so loop from i=1 to 5-3=2, so i=1,2
        verify(mockStatement).setObject(2, "value1");
        verify(mockStatement).setObject(3, "value2");
        // Verify executeUpdate was called
        verify(mockStatement).executeUpdate();
    }

    @Test
    public void testSqlExecuteWithEmptyStringFirstElement() throws SQLException {
        // Test case where newRow[0] is empty string - should execute SQL
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {"", "value1", "value2", "value3", "value4"};

        BaseTrigger.sqlExecute(newRow, mockStatement);

        // Verify that setObject was called for the UUID at position 1
        verify(mockStatement).setObject(eq(1), any(String.class));
        // Verify that setObject was called for each element in the loop
        verify(mockStatement).setObject(2, "value1");
        verify(mockStatement).setObject(3, "value2");
        // Verify executeUpdate was called
        verify(mockStatement).executeUpdate();
    }

    @Test
    public void testSqlExecuteWithNonEmptyFirstElement() throws SQLException {
        // Test case where newRow[0] is not empty - should do nothing
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {"not-empty", "value1", "value2", "value3", "value4"};

        BaseTrigger.sqlExecute(newRow, mockStatement);

        // Verify that no methods were called on preparedStatement
        verify(mockStatement, never()).setObject(anyInt(), any());
        verify(mockStatement, never()).executeUpdate();
    }

    @Test
    public void testSqlExecuteWithMinimumArrayLength() throws SQLException {
        // Test with minimum array length where loop doesn't execute
        // newRow.length = 3, loop from i=1 to 3-3=0, so no loop iterations
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {null, "value1", "value2"};

        BaseTrigger.sqlExecute(newRow, mockStatement);

        // Verify UUID was set
        verify(mockStatement).setObject(eq(1), any(String.class));
        // Verify no setObject calls in the loop (since length-2 = 3-2 = 1, loop doesn't run)
        verify(mockStatement, never()).setObject(eq(2), any());
        verify(mockStatement, never()).setObject(eq(3), any());
        // Verify executeUpdate was called
        verify(mockStatement).executeUpdate();
    }

    @Test
    public void testSqlExecuteWithLargerArray() throws SQLException {
        // Test with larger array
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {null, "val1", "val2", "val3", "val4", "val5", "val6"};

        BaseTrigger.sqlExecute(newRow, mockStatement);

        // Verify UUID was set
        verify(mockStatement).setObject(eq(1), any(String.class));
        // newRow.length = 7, loop from i=1 to 7-3=4, so i=1,2,3,4
        verify(mockStatement).setObject(2, "val1");
        verify(mockStatement).setObject(3, "val2");
        verify(mockStatement).setObject(4, "val3");
        verify(mockStatement).setObject(5, "val4");
        // i=5 would be 7-2=5, but loop condition is i < length-2, so i<5, stops at i=4
        verify(mockStatement, never()).setObject(eq(6), any());
        verify(mockStatement, never()).setObject(eq(7), any());
        // Verify executeUpdate was called
        verify(mockStatement).executeUpdate();
    }

    @Test
    public void testSqlExecuteWithDifferentDataTypes() throws SQLException {
        // Test with different data types in the array
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {null, "string", 123, true, 45.67};

        BaseTrigger.sqlExecute(newRow, mockStatement);

        // Verify UUID was set
        verify(mockStatement).setObject(eq(1), any(String.class));
        // newRow.length = 5, loop from i=1 to 5-3=2, so i=1,2
        verify(mockStatement).setObject(2, "string");
        verify(mockStatement).setObject(3, 123);
        // Verify executeUpdate was called
        verify(mockStatement).executeUpdate();
    }

    @Test
    public void testSqlExecuteThrowsSQLException() throws SQLException {
        // Test that SQLException is propagated
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {null, "value1"};

        // Mock PreparedStatement to throw SQLException
        doThrow(new SQLException("Test exception")).when(mockStatement).executeUpdate();

        Assertions.assertThrows(SQLException.class, () -> {
            BaseTrigger.sqlExecute(newRow, mockStatement);
        });
    }

    @Test
    public void testSqlExecuteWithEmptyArray() throws SQLException {
        // Test edge case - this would cause ArrayIndexOutOfBoundsException
        // but let's see what happens with minimal array
        PreparedStatement mockStatement = mock(PreparedStatement.class);
        Object[] newRow = {null};

        BaseTrigger.sqlExecute(newRow, mockStatement);

        // Should still work since we only access newRow[0] and check length
        verify(mockStatement).setObject(eq(1), any(String.class));
        // length = 1, loop from i=1 to 1-3=-2, so no loop iterations
        verify(mockStatement, never()).setObject(eq(2), any());
        verify(mockStatement).executeUpdate();
    }
}
