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

package org.apache.shenyu.plugin.ai.pii.profile;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * One decoded string value inside a parsed tool or function call
 * {@code arguments} document, created by {@link ToolCallArguments}. The text is
 * the decoded value, so a detector sees the identifier the tool recovers rather
 * than its JSON escaping. A replacement writes the redacted value back into the
 * shared parsed document and re serializes the whole document into the member,
 * so the sibling values and the surrounding structure are preserved and the
 * member stays valid JSON.
 */
final class ArgumentValueSlot implements PiiTextSlot {

    private final JsonObject holder;

    private final String member;

    private final JsonElement root;

    private final Supplier<String> read;

    private final Consumer<String> write;

    ArgumentValueSlot(final JsonObject holder, final String member, final JsonElement root,
                      final Supplier<String> read, final Consumer<String> write) {
        this.holder = holder;
        this.member = member;
        this.root = root;
        this.read = read;
        this.write = write;
    }

    @Override
    public String text() {
        return read.get();
    }

    @Override
    public void replace(final String newText) {
        write.accept(newText);
        // re serialize the whole parsed document, so the sibling values that share
        // this root and the structure around the redacted value are preserved
        holder.addProperty(member, root.toString());
    }
}
