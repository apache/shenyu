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

import java.lang.annotation.Annotation;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import org.dromara.soul.common.utils.ObjectUtils;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.Opcodes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * AnnotationMethodVisitor
 *
 * @author sixh
 */
final class AnnotationReadingVisitor extends AnnotationVisitor {

    private final Logger logger = LoggerFactory.getLogger(AnnotationReadingVisitor.class);

    private final Map<String, Set<String>> metaAnnotationMap;

    private final ClassLoader classLoader;

    private final String annotationType;

    private final Class<? extends Annotation> annotation;

    AnnotationReadingVisitor(String annotationType, Map<String, Set<String>> metaAnnotationMap,
                             ClassLoader classLoader) {
        super(Opcodes.ASM7);
        this.annotationType = annotationType;
        this.metaAnnotationMap = metaAnnotationMap;
        this.classLoader = classLoader;
        this.annotation = this.getAnnotationClass();
    }

    @SuppressWarnings("all")
    private Class<? extends Annotation> getAnnotationClass() {
        if (classLoader != null && annotationType != null) {
            try {
                return (Class<? extends Annotation>) classLoader.loadClass(annotationType);
            } catch (ClassNotFoundException e) {

            }
        }
        return null;
    }

    @Override
    public void visitEnd() {
        super.visitEnd();
        if (annotation == null) {
            return;
        }
        Class<? extends Annotation> annotationClass = this.annotation;
        if (!isInJavaLangAnnotationPackage(annotationClass.getName())) {
            try {
                Annotation[] metaAnnotations = annotationClass.getAnnotations();
                if (!ObjectUtils.isEmpty(metaAnnotations)) {
                    Set<Annotation> visited = new LinkedHashSet<>();
                    for (Annotation metaAnnotation : metaAnnotations) {
                        recursivelyCollectMetaAnnotations(visited, metaAnnotation);
                    }
                    if (!visited.isEmpty()) {
                        Set<String> metaAnnotationTypeNames = new LinkedHashSet<>(visited.size());
                        for (Annotation ann : visited) {
                            metaAnnotationTypeNames.add(ann.annotationType().getName());
                        }
                        this.metaAnnotationMap.put(annotationClass.getName(), metaAnnotationTypeNames);
                    }
                }
            } catch (Throwable ex) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Failed to introspect meta-annotations on " + annotationClass + ": " + ex);
                }
            }
        }
    }

    private void recursivelyCollectMetaAnnotations(Set<Annotation> visited, Annotation annotation) {
        Class<? extends Annotation> annotationType = annotation.annotationType();
        String annotationName = annotationType.getName();
        if (!isInJavaLangAnnotationPackage(annotationName) && visited.add(annotation)) {
            try {
                for (Annotation metaMetaAnnotation : annotationType.getAnnotations()) {
                    recursivelyCollectMetaAnnotations(visited, metaMetaAnnotation);
                }
            } catch (Throwable ex) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Failed to introspect meta-annotations on " + annotation + ": " + ex);
                }
            }
        }
    }

    private boolean isInJavaLangAnnotationPackage(String annotationType) {
        return (annotationType != null && annotationType.startsWith("java.lang.annotation"));
    }

    public Map<String, Set<String>> getMetaAnnotationMap() {
        return metaAnnotationMap;
    }

    public ClassLoader getClassLoader() {
        return classLoader;
    }

    public String getAnnotationType() {
        return annotationType;
    }
}
