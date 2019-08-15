/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.api.yaml;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.BaseConstructor;
import org.yaml.snakeyaml.constructor.Constructor;
import org.yaml.snakeyaml.nodes.*;
import org.yaml.snakeyaml.representer.Representer;
import org.yaml.snakeyaml.resolver.Resolver;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Class to load {@code .yml} files into a map of {@code String} to
 *
 * @author Madhura Bhave
 * @author Phillip Webb
 */
class OriginTrackedYamlLoader extends YamlProcessor {

    private final InputStream resource;

    OriginTrackedYamlLoader(InputStream resource) {
        this.resource = resource;
        setResources(resource);
    }

    @Override
    protected Yaml createYaml() {
        BaseConstructor constructor = new OriginTrackingConstructor();
        Representer representer = new Representer();
        DumperOptions dumperOptions = new DumperOptions();
        LimitedResolver resolver = new LimitedResolver();
        LoaderOptions loaderOptions = new LoaderOptions();
        loaderOptions.setAllowDuplicateKeys(false);
        return new Yaml(constructor, representer, dumperOptions, loaderOptions, resolver);
    }

    public List<Map<String, Object>> load() {
        final List<Map<String, Object>> result = new ArrayList<>();
        process((properties, map) -> result.add(getFlattenedMap(map)));
        return result;
    }

    /**
     * {@link Constructor} that tracks property origins.
     */
    private class OriginTrackingConstructor extends Constructor {

        @Override
        protected Object constructObject(Node node) {
            if (node instanceof ScalarNode) {
                if (!(node instanceof KeyScalarNode)) {
                    return constructTrackedObject(node, super.constructObject(node));
                }
            } else if (node instanceof MappingNode) {
                replaceMappingNodeKeys((MappingNode) node);
            }
            return super.constructObject(node);
        }

        private void replaceMappingNodeKeys(MappingNode node) {
            node.setValue(node.getValue().stream().map(KeyScalarNode::get)
                    .collect(Collectors.toList()));
        }

        private Object constructTrackedObject(Node node, Object value) {
            return value;
        }

        private Object getValue(Object value) {
            return (value != null ? value : "");
        }

    }

    /**
     * {@link ScalarNode} that replaces the key node in a {@link NodeTuple}.
     */
    private static class KeyScalarNode extends ScalarNode {

        KeyScalarNode(ScalarNode node) {
            super(node.getTag(), node.getValue(), node.getStartMark(), node.getEndMark(),
                    node.getStyle());
        }

        public static NodeTuple get(NodeTuple nodeTuple) {
            Node keyNode = nodeTuple.getKeyNode();
            Node valueNode = nodeTuple.getValueNode();
            return new NodeTuple(KeyScalarNode.get(keyNode), valueNode);
        }

        private static Node get(Node node) {
            if (node instanceof ScalarNode) {
                return new KeyScalarNode((ScalarNode) node);
            }
            return node;
        }

    }

    /**
     * {@link Resolver} that limits {@link Tag#TIMESTAMP} tags.
     */
    private static class LimitedResolver extends Resolver {

        @Override
        public void addImplicitResolver(Tag tag, Pattern regexp, String first) {
            if (tag == Tag.TIMESTAMP) {
                return;
            }
            super.addImplicitResolver(tag, regexp, first);
        }

    }

}
