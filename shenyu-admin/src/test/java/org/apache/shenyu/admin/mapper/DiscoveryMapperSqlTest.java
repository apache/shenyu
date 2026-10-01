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

package org.apache.shenyu.admin.mapper;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.io.StringReader;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Regression tests for the discovery mapper SQL definitions.
 */
public final class DiscoveryMapperSqlTest {

    @Test
    public void selectBySelectorNameAndPluginNameUsesSelectorNameColumn() throws Exception {
        Document document = loadMapperDocument();
        NodeList statements = document.getElementsByTagName("select");
        String statement = null;
        for (int i = 0; i < statements.getLength(); i++) {
            if ("selectBySelectorNameAndPluginName".equals(statements.item(i).getAttributes().getNamedItem("id").getNodeValue())) {
                statement = statements.item(i).getTextContent();
                break;
            }
        }
        assertNotNull(statement);
        assertTrue(statement.contains("s.selector_name = #{selectorName}"));
        assertTrue(statement.contains("d.plugin_name = #{pluginName}"));
        assertFalse(statement.contains("s.name = #{selectorName}"));
    }

    private Document loadMapperDocument() throws Exception {
        try (InputStream input = getClass().getClassLoader().getResourceAsStream("mappers/discovery-sqlmap.xml")) {
            assertNotNull(input);
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
            DocumentBuilder builder = factory.newDocumentBuilder();
            builder.setEntityResolver((publicId, systemId) -> new InputSource(new StringReader("")));
            return builder.parse(input);
        }
    }
}
