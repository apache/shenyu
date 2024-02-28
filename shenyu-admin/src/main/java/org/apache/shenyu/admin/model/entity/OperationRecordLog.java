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

package org.apache.shenyu.admin.model.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;

import java.util.Date;

/**
 * operation_record_log.
 */
public class OperationRecordLog {
    
    /**
     * id.
     */
    @JsonSerialize(using = ToStringSerializer.class)
    private Long id;
    
    /**
     * color.
     */
    private String color;
    
    /**
     * context.
     */
    private String context;
    
    /**
     * operator.
     */
    private String operator;
    
    /**
     * operation time.
     */
    @JsonFormat(pattern = "MM-dd HH:mm:ss")
    private Date operationTime;
    
    /**
     * operation type.
     */
    private String operationType;
    
    /**
     * get id.
     *
     * @return id
     */
    public Long getId() {
        return id;
    }
    
    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final Long id) {
        this.id = id;
    }
    
    /**
     * get color.
     *
     * @return color
     */
    public String getColor() {
        return color;
    }
    
    /**
     * set color.
     *
     * @param color color
     */
    public void setColor(final String color) {
        this.color = color;
    }
    
    /**
     * get context.
     *
     * @return context
     */
    public String getContext() {
        return context;
    }
    
    /**
     * set context.
     *
     * @param context context
     */
    public void setContext(final String context) {
        this.context = context;
    }
    
    /**
     * get operator.
     *
     * @return operator
     */
    public String getOperator() {
        return operator;
    }
    
    /**
     * set operator.
     *
     * @param operator operator
     */
    public void setOperator(final String operator) {
        this.operator = operator;
    }
    
    /**
     * get operationTime.
     *
     * @return operationTime
     */
    public Date getOperationTime() {
        return operationTime;
    }
    
    /**
     * set operationTime.
     *
     * @param operationTime operationTime
     */
    public void setOperationTime(final Date operationTime) {
        this.operationTime = operationTime;
    }
    
    /**
     * get operationType.
     *
     * @return operationType
     */
    public String getOperationType() {
        return operationType;
    }
    
    /**
     * set operationType.
     *
     * @param operationType operationType
     */
    public void setOperationType(final String operationType) {
        this.operationType = operationType;
    }
    
    @Override
    public String toString() {
        return "OperationRecordLog{"
                + "id=" + id
                + ", color='" + color + '\''
                + ", context='" + context + '\''
                + ", operator='" + operator + '\''
                + ", operationTime=" + operationTime
                + ", operationType='" + operationType + '\''
                + '}';
    }
}
