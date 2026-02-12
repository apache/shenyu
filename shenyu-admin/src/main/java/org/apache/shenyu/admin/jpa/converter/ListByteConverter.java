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

package org.apache.shenyu.admin.jpa.converter;

import com.google.gson.reflect.TypeToken;
import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.List;
import java.util.Objects;


@Converter
public class ListByteConverter implements AttributeConverter<List<Byte>, String> {

    @Override
    public String convertToDatabaseColumn(final List<Byte> attribute) {
        return Objects.isNull(attribute) ? null : GsonUtils.getGson().toJson(attribute);
    }

    @Override
    public List<Byte> convertToEntityAttribute(final String dbData) {
        return Objects.isNull(dbData) ? null : GsonUtils.getGson().fromJson(dbData, new TypeToken<List<Byte>>() {
        }.getType());
    }
}
