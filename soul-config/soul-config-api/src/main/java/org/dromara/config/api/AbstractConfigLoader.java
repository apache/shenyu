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
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Optional;

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
                URL url = loader.getResource(filePath);
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
            Yaml yaml = new Yaml();
            Config baseConfig = yaml.load(inputStream);
            return load0(baseConfig);
        } catch (IOException e) {
            throw new SoulException("ConfigLoader:loader config error,file path:" + filePath);
        }
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
