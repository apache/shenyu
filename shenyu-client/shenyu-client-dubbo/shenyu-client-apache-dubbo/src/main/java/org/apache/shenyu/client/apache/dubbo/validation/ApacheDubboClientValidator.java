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

package org.apache.shenyu.client.apache.dubbo.validation;

import java.util.Objects;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtField;
import javassist.CtNewConstructor;
import javassist.NotFoundException;
import javassist.bytecode.AnnotationsAttribute;
import javassist.bytecode.ClassFile;
import javassist.bytecode.ConstPool;
import javassist.bytecode.annotation.ArrayMemberValue;
import javassist.bytecode.annotation.BooleanMemberValue;
import javassist.bytecode.annotation.ByteMemberValue;
import javassist.bytecode.annotation.CharMemberValue;
import javassist.bytecode.annotation.ClassMemberValue;
import javassist.bytecode.annotation.DoubleMemberValue;
import javassist.bytecode.annotation.EnumMemberValue;
import javassist.bytecode.annotation.FloatMemberValue;
import javassist.bytecode.annotation.IntegerMemberValue;
import javassist.bytecode.annotation.LongMemberValue;
import javassist.bytecode.annotation.MemberValue;
import javassist.bytecode.annotation.ShortMemberValue;
import javassist.bytecode.annotation.StringMemberValue;
import org.apache.commons.lang3.StringUtils;
import org.apache.dubbo.common.URL;
import org.apache.dubbo.common.bytecode.ClassGenerator;
import org.apache.dubbo.common.utils.ReflectUtils;
import org.apache.dubbo.validation.MethodValidated;
import org.apache.dubbo.validation.Validator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.validation.Constraint;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.ValidationException;
import jakarta.validation.ValidatorFactory;
import jakarta.validation.groups.Default;
import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * ApacheDubboClientValidator.
 */
public class ApacheDubboClientValidator implements Validator {

    private static final Logger LOG = LoggerFactory.getLogger(ApacheDubboClientValidator.class);

    private final Class<?> clazz;

    private final Map<String, Class<?>> methodClassMap;

    private final jakarta.validation.Validator validator;

    @SuppressWarnings({"unchecked", "rawtypes"})
    public ApacheDubboClientValidator(final URL url) {
        this.clazz = ReflectUtils.forName(url.getServiceInterface());
        String shenyuValidation = url.getParameter("shenyuValidation");
        ValidatorFactory factory;
        if (StringUtils.isNoneBlank(shenyuValidation)) {
            factory = Validation.byProvider((Class) ReflectUtils.forName(shenyuValidation)).configure().buildValidatorFactory();
        } else {
            factory = Validation.buildDefaultValidatorFactory();
        }
        this.validator = factory.getValidator();
        this.methodClassMap = new ConcurrentHashMap<>();
    }

    private static Object getMethodParameterBean(final Class<?> clazz, final Method method, final Object[] args) {
        if (!hasConstraintParameter(method)) {
            return null;
        }
        try {
            String parameterClassName = generateMethodParameterClassName(clazz, method);
            Class<?> parameterClass;
            try {
                parameterClass = Class.forName(parameterClassName, true, clazz.getClassLoader());
            } catch (ClassNotFoundException e) {
                parameterClass = generateMethodParameterClass(clazz, method, parameterClassName);
            }
            Object parameterBean = parameterClass.newInstance();
            for (int i = 0; i < args.length; i++) {
                Field field = parameterClass.getField(method.getName() + "Argument" + i);
                field.set(parameterBean, args[i]);
            }
            return parameterBean;
        } catch (Exception e) {
            LOG.warn(e.getMessage(), e);
            return null;
        }
    }

    /**
     * try to generate methodParameterClass.
     *
     * @param clazz              interface class
     * @param method             invoke method
     * @param parameterClassName generated parameterClassName
     * @return Class<?> generated methodParameterClass
     */
    private static Class<?> generateMethodParameterClass(final Class<?> clazz, final Method method, final String parameterClassName)
            throws Exception {
        ClassPool pool = ClassGenerator.getClassPool(clazz.getClassLoader());
        synchronized (parameterClassName.intern()) {
            CtClass ctClass = null;
            try {
                ctClass = pool.getCtClass(parameterClassName);
            } catch (NotFoundException neglect) {
                LOG.error(neglect.getMessage(), neglect);
            }

            if (Objects.isNull(ctClass)) {
                ctClass = pool.makeClass(parameterClassName);
                ClassFile classFile = ctClass.getClassFile();
                classFile.setVersionToJava5();
                ctClass.addConstructor(CtNewConstructor.defaultConstructor(pool.getCtClass(parameterClassName)));
                // parameter fields
                Class<?>[] parameterTypes = method.getParameterTypes();
                Annotation[][] parameterAnnotations = method.getParameterAnnotations();
                for (int i = 0; i < parameterTypes.length; i++) {
                    Class<?> type = parameterTypes[i];
                    Annotation[] annotations = parameterAnnotations[i];
                    AnnotationsAttribute attribute = new AnnotationsAttribute(classFile.getConstPool(), AnnotationsAttribute.visibleTag);
                    for (Annotation annotation : annotations) {
                        if (annotation.annotationType().isAnnotationPresent(Constraint.class)) {
                            javassist.bytecode.annotation.Annotation ja = new javassist.bytecode.annotation.Annotation(
                                    classFile.getConstPool(), pool.getCtClass(annotation.annotationType().getName()));
                            Method[] members = annotation.annotationType().getMethods();
                            for (Method member : members) {
                                if (Modifier.isPublic(member.getModifiers())
                                        && member.getParameterTypes().length == 0
                                        && member.getDeclaringClass() == annotation.annotationType()) {
                                    Object value = member.invoke(annotation);
                                    if (Objects.nonNull(value)) {
                                        MemberValue memberValue = createMemberValue(
                                                classFile.getConstPool(), pool.get(member.getReturnType().getName()), value);
                                        ja.addMemberValue(member.getName(), memberValue);
                                    }
                                }
                            }
                            attribute.addAnnotation(ja);
                        }
                    }
                    String fieldName = method.getName() + "Argument" + i;
                    CtField ctField = CtField.make("public " + type.getCanonicalName() + " " + fieldName + ";", pool.getCtClass(parameterClassName));
                    ctField.getFieldInfo().addAttribute(attribute);
                    ctClass.addField(ctField);
                }
                return ctClass.toClass(clazz.getClassLoader(), null);
            } else {
                return Class.forName(parameterClassName, true, clazz.getClassLoader());
            }
        }
    }

    private static String generateMethodParameterClassName(final Class<?> clazz, final Method method) {
        StringBuilder builder = new StringBuilder().append(clazz.getName())
                .append("_")
                .append(toUpperMethoName(method.getName()))
                .append("Parameter");

        Class<?>[] parameterTypes = method.getParameterTypes();
        for (Class<?> parameterType : parameterTypes) {
            builder.append("_").append(parameterType.getName());
        }

        return builder.toString();
    }

    private static boolean hasConstraintParameter(final Method method) {
        Annotation[][] parameterAnnotations = method.getParameterAnnotations();
        for (Annotation[] annotations : parameterAnnotations) {
            for (Annotation annotation : annotations) {
                if (annotation.annotationType().isAnnotationPresent(Constraint.class)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static String toUpperMethoName(final String methodName) {
        return methodName.substring(0, 1).toUpperCase() + methodName.substring(1);
    }

    // Copy from javassist.bytecode.annotation.Annotation.createMemberValue(ConstPool, CtClass);
    private static MemberValue createMemberValue(final ConstPool cp, final CtClass type, final Object value) throws NotFoundException {
        MemberValue memberValue = javassist.bytecode.annotation.Annotation.createMemberValue(cp, type);
        if (memberValue instanceof BooleanMemberValue) {
            ((BooleanMemberValue) memberValue).setValue((Boolean) value);
        } else if (memberValue instanceof ByteMemberValue) {
            ((ByteMemberValue) memberValue).setValue((Byte) value);
        } else if (memberValue instanceof CharMemberValue) {
            ((CharMemberValue) memberValue).setValue((Character) value);
        } else if (memberValue instanceof ShortMemberValue) {
            ((ShortMemberValue) memberValue).setValue((Short) value);
        } else if (memberValue instanceof IntegerMemberValue) {
            ((IntegerMemberValue) memberValue).setValue((Integer) value);
        } else if (memberValue instanceof LongMemberValue) {
            ((LongMemberValue) memberValue).setValue((Long) value);
        } else if (memberValue instanceof FloatMemberValue) {
            ((FloatMemberValue) memberValue).setValue((Float) value);
        } else if (memberValue instanceof DoubleMemberValue) {
            ((DoubleMemberValue) memberValue).setValue((Double) value);
        } else if (memberValue instanceof ClassMemberValue) {
            ((ClassMemberValue) memberValue).setValue(((Class<?>) value).getName());
        } else if (memberValue instanceof StringMemberValue) {
            ((StringMemberValue) memberValue).setValue((String) value);
        } else if (memberValue instanceof EnumMemberValue) {
            ((EnumMemberValue) memberValue).setValue(((Enum<?>) value).name());
            /* else if (memberValue instanceof AnnotationMemberValue) */
        } else if (memberValue instanceof ArrayMemberValue) {
            CtClass arrayType = type.getComponentType();
            int len = Array.getLength(value);
            MemberValue[] members = new MemberValue[len];
            for (int i = 0; i < len; i++) {
                members[i] = createMemberValue(cp, arrayType, Array.get(value, i));
            }
            ((ArrayMemberValue) memberValue).setValue(members);
        }
        return memberValue;
    }

    @Override
    public void validate(final String methodName, final Class<?>[] parameterTypes, final Object[] arguments) throws Exception {
        List<Class<?>> groups = new ArrayList<>();
        Class<?> methodClass = methodClass(methodName);
        if (Objects.nonNull(methodClass)) {
            groups.add(methodClass);
        }
        Set<ConstraintViolation<?>> violations = new HashSet<>();
        Method method = clazz.getMethod(methodName, parameterTypes);
        Class<?>[] methodClasses;
        if (method.isAnnotationPresent(MethodValidated.class)) {
            methodClasses = method.getAnnotation(MethodValidated.class).value();
            groups.addAll(Arrays.asList(methodClasses));
        }
        // add into default group
        groups.add(0, Default.class);
        groups.add(1, clazz);

        // convert list to array
        Class<?>[] classGroups = new Class<?>[groups.size()];
        classGroups = groups.toArray(classGroups);

        Object parameterBean = getMethodParameterBean(clazz, method, arguments);
        if (Objects.nonNull(parameterBean)) {
            violations.addAll(validator.validate(parameterBean, classGroups));
        }

        for (Object arg : arguments) {
            validate(violations, arg, classGroups);
        }

        if (!violations.isEmpty()) {
            LOG.error("Failed to validate service: {}, method: {}, cause: {}", clazz.getName(), methodName, violations);
            StringBuilder validateError = new StringBuilder();
            violations.forEach(each -> validateError.append(each.getMessage()).append(","));
            throw new ValidationException(validateError.substring(0, validateError.length() - 1));
        }
    }

    private void validate(final Set<ConstraintViolation<?>> violations, final Object arg, final Class<?>... groups) {
        if (Objects.nonNull(arg) && !ReflectUtils.isPrimitives(arg.getClass())) {
            if (arg instanceof Object[]) {
                for (Object item : (Object[]) arg) {
                    validate(violations, item, groups);
                }
            } else if (arg instanceof Collection) {
                for (Object item : (Collection<?>) arg) {
                    validate(violations, item, groups);
                }
            } else if (arg instanceof Map) {
                for (Map.Entry<?, ?> entry : ((Map<?, ?>) arg).entrySet()) {
                    validate(violations, entry.getKey(), groups);
                    validate(violations, entry.getValue(), groups);
                }
            } else {
                violations.addAll(validator.validate(arg, groups));
            }
        }
    }

    private Class<?> methodClass(final String methodName) {
        Class<?> methodClass = null;
        String methodClassName = clazz.getName() + "$" + toUpperMethoName(methodName);
        Class<?> cached = methodClassMap.get(methodClassName);
        if (Objects.nonNull(cached)) {
            return cached == clazz ? null : cached;
        }
        try {
            methodClass = Class.forName(methodClassName, false, Thread.currentThread().getContextClassLoader());
            methodClassMap.put(methodClassName, methodClass);
        } catch (ClassNotFoundException e) {
            methodClassMap.put(methodClassName, clazz);
        }
        return methodClass;
    }
}
