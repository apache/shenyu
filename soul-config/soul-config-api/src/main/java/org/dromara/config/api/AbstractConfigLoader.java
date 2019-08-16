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

import org.dromara.config.api.property.PropertyKeySource;
import org.dromara.config.api.yaml.YamlPropertyLoader;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.StringUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * ParentConfigLoader .
 * Read basic BaseConfig information processing.
 *
 * @author sixh
 */
public abstract class AbstractConfigLoader implements ConfigLoader {

    private List<PropertyLoader> loaders = new ArrayList<>();

    {
        loaders.add(new YamlPropertyLoader());
    }

    @Override
    public BaseConfig load() {
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
                if (url != null) {
                    filePath = url.getFile();
                    configFile = new File(filePath);
                } else {
                    throw new SoulException("ConfigLoader:loader config error,error file path:" + filePath);
                }
            }
        } else {
            configFile = new File(filePath);
            if (!configFile.exists()) {
                throw new SoulException("ConfigLoader:loader config error,error file path:" + filePath);
            }
        }
        try (FileInputStream inputStream = new FileInputStream(configFile)) {
            for (PropertyLoader loader : loaders) {
                List<PropertyKeySource<?>> load = loader.load(filePath, inputStream);
                System.out.println(load);
            }
        } catch (IOException e) {
            throw new SoulException("ConfigLoader:loader config error,file path:" + filePath);
        }
        return new BaseConfig();
    }


    /**
     * Configuration information loaded by different configuration centers of subclass implementations.
     *
     * @param config Loaded basic configuration information.
     * @return Completed configuration information.
     */
    public abstract BaseConfig load0(List<PropertyKeySource<?>> config);

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
