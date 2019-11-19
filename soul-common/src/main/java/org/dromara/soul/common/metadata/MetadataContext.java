/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.common.metadata;

import com.google.common.collect.HashMultimap;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import org.dromara.soul.common.extension.JoinConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * AnnotationContext
 * ave the parsed information of the source annotation.
 *
 * @author sixh
 */
public enum MetadataContext {
    /**
     * Instance metadata context.
     */
    INSTANCE;

    private final Logger logger = LoggerFactory.getLogger(MetadataContext.class);

    private final Set<String> baseAnnotation = new HashSet<>();

    {
        baseAnnotation.add(JoinConfig.class.getName());
    }

    /**
     * Process of saving source annotation information.
     */
    private HashMultimap<String, MetadataReader> cache = HashMultimap.create();

    /**
     * Start scanning.
     *
     * @param baseScanValues the base scan.
     */
    public void scan(Set<String> baseScanValues) {
        Set<String> baseScanSets;
        if (baseScanValues != null && !baseScanValues.isEmpty()) {
            baseScanSets = baseScanValues;
        } else {
            baseScanSets = new HashSet<>();
        }
        String base = "org.dromara.soul";
        baseScanSets.add(base);
        ClassLoader classLoader = this.getClass().getClassLoader();
        PackageScanner classPathPackageScanner;
        for (String baseScan : baseScanSets) {
            classPathPackageScanner = new ClassPathPackageScanner(baseScan);
            try {
                Set<Resource> fullClass = classPathPackageScanner.getFullClass();
                for (Resource aClass : fullClass) {
                    MetadataReader reader = SimpleMetadataReaderFactory.INSTALL.reader(aClass, classLoader);
                    baseAnnotation(reader);
                }
            } catch (IOException e) {
                logger.warn("scan {} error", baseScan, e);
            }
        }
        SimpleMetadataReaderFactory.INSTALL.removeAll();
    }

    /**
     * Gets annotation class.
     *
     * @param clazz the clazz
     * @return the annotation class.
     */
    public Set<Class<?>> getAnnotationClass(Class<? extends Annotation> clazz) {
        String className = clazz.getName();
        if (baseAnnotation.contains(className)) {
            Set<Class<?>> classes = new HashSet<>();
            Set<MetadataReader> metadataReaders = cache.get(className);
            for (MetadataReader metadataReader : metadataReaders) {
                if (metadataReader.getAnnotationMetadata().hasAnnotation(className)) {
                    String classReader = metadataReader.getClassMetadata().getClassName();
                    try {
                        Class<?> aClass = Class.forName(classReader);
                        classes.add(aClass);
                    } catch (ClassNotFoundException e) {
                        logger.info("class not found{}", className);
                    }
                }
            }
            return classes;
        }
        return Collections.emptySet();
    }

    /**
     * Base annotation.
     *
     * @param reader the reader.
     */
    private void baseAnnotation(MetadataReader reader) {
        if (reader.getAnnotationMetadata().getAnnotationTypes().isEmpty()) {
            return;
        }
        Set<String> annotationTypes = reader.getAnnotationMetadata().getAnnotationTypes();
        for (String annotationType : annotationTypes) {
            boolean contains = baseAnnotation.contains(annotationType);
            ClassMetadata classMetadata = reader.getClassMetadata();
            if (contains && classMetadata.isConcrete() && !classMetadata.isAbstract()) {
                cache.put(annotationType, reader);
                logger.info("scan class {}", reader.getClassMetadata().getClassName());
            }
        }
    }
}
