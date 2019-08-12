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

package org.dromara.config.api;

import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.StringUtils;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.reader.UnicodeReader;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Reader;
import java.net.URL;
import java.util.*;

/**
 * ParentConfigLoader .
 * Read basic Config information processing.
 *
 * @author sixh
 */
public abstract class AbstractConfigLoader implements ConfigLoader {

    @Override
    public Config load() {
        String filePath = System.getProperty("soul.conf");
        File configFile;
        if (StringUtils.isBlank(filePath)) {
            String dirPath = getDirGlobal();
            configFile = new File(dirPath);
            if (configFile.exists()) {
                filePath = dirPath;
                throw new SoulException("ConfigLoader:loader config error,error file path:" + filePath);
            } else {
                //Mainly used for development environment。
                ClassLoader loader = ConfigLoader.class.getClassLoader();
                URL url = loader.getResource("soul.yml");
                configFile = Optional.ofNullable(url).map(e -> {
                    String file = url.getFile();
                    return new File(file);
                }).orElseThrow(() ->
                        new SoulException("ConfigLoader:loader config error,error file soul.yml"));
            }
        } else {
            configFile = new File(filePath);
            if (!configFile.exists()) {
                throw new SoulException("ConfigLoader:loader config error,error file path:" + filePath);
            }
        }
        try (FileInputStream inputStream = new FileInputStream(configFile)) {
            LoaderOptions options = new LoaderOptions();
            options.setAllowDuplicateKeys(false);
            Yaml yaml = new Yaml(options);
            Reader reader = new UnicodeReader(inputStream);
            yaml.loadAll(reader).forEach(e -> {
                loadYmal(e);
            });
        } catch (IOException e) {
            throw new SoulException("ConfigLoader:loader config error,file path:" + filePath);
        }
        return new Config();
    }

    protected  void loadYmal(Object object) {
        Map<String, Object> result = new LinkedHashMap<>();
        buildFlattenedMap(result, asMap(object), null);
        System.out.println(result);
    }

    private Map<String, Object> asMap(Object object) {
        // YAML can have numbers as keys
        Map<String, Object> result = new LinkedHashMap<>();
        if (!(object instanceof Map)) {
            // A document can be a text literal
            result.put("document", object);
            return result;
        }

        Map<Object, Object> map = (Map<Object, Object>) object;
        map.forEach((key, value) -> {
            if (value instanceof Map) {
                value = asMap(value);
            }
            if (key instanceof CharSequence) {
                result.put(key.toString(), value);
            }
            else {
                // It has to be a map key in this case
                result.put("[" + key.toString() + "]", value);
            }
        });
        return result;
    }

    private void buildFlattenedMap(Map<String, Object> result, Map<String, Object> source, String path) {
        source.forEach((key, value) -> {
            if (StringUtils.isNotBlank(path)) {
                if (key.startsWith("[")) {
                    key = path + key;
                }
                else {
                    key = path + '.' + key;
                }
            }
            if (value instanceof String) {
                result.put(key, value);
            }
            else if (value instanceof Map) {
                // Need a compound key
                @SuppressWarnings("unchecked")
                Map<String, Object> map = (Map<String, Object>) value;
                buildFlattenedMap(result, map, key);
            }
            else if (value instanceof Collection) {
                // Need a compound key
                @SuppressWarnings("unchecked")
                Collection<Object> collection = (Collection<Object>) value;
                if (collection.isEmpty()) {
                    result.put(key, "");
                }
                else {
                    int count = 0;
                    for (Object object : collection) {
                        buildFlattenedMap(result, Collections.singletonMap(
                                "[" + (count++) + "]", object), key);
                    }
                }
            }
            else {
                result.put(key, (value != null ? value : ""));
            }
        });
    }
    /**
     * Configuration information loaded by different configuration centers of subclass implementations.
     *
     * @param config Loaded basic configuration information.
     * @return Completed configuration information.
     */
    public abstract Config load0(Config config);

    /**
     * Get the current project path.
     *
     * @return Current project path
     */
    private String getDirGlobal() {
        String userDir = System.getProperty("user.dir");
        String fileName = "soul.yml";
        return String.join(String.valueOf(File.separatorChar), userDir, fileName);
    }
}
