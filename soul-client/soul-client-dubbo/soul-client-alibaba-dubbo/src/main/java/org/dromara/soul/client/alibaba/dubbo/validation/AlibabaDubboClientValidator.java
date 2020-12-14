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

package org.dromara.soul.client.alibaba.dubbo.validation;

import com.alibaba.dubbo.common.URL;
import com.alibaba.dubbo.common.bytecode.ClassGenerator;
import com.alibaba.dubbo.common.logger.Logger;
import com.alibaba.dubbo.common.logger.LoggerFactory;
import com.alibaba.dubbo.common.utils.ReflectUtils;
import com.alibaba.dubbo.validation.MethodValidated;
import com.alibaba.dubbo.validation.Validator;
import com.alibaba.dubbo.validation.support.jvalidation.JValidator;
import javassist.*;
import javassist.bytecode.AnnotationsAttribute;
import javassist.bytecode.ClassFile;
import javassist.bytecode.ConstPool;
import javassist.bytecode.annotation.*;

import javax.validation.*;
import javax.validation.groups.Default;
import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.security.ProtectionDomain;
import java.util.*;

/**
 * AlibabaDubboClientValidator
 *
 * @author KevinClair
 */
public class AlibabaDubboClientValidator implements Validator {
    private static final Logger logger = LoggerFactory.getLogger(JValidator.class);
    private final Class<?> clazz;
    private final javax.validation.Validator validator;

    public AlibabaDubboClientValidator(URL url) {
        this.clazz = ReflectUtils.forName(url.getServiceInterface());
        String soulValidation = url.getParameter("soulValidation");
        ValidatorFactory factory;
        if (soulValidation != null && soulValidation.length() > 0) {
            factory = Validation.byProvider((Class) ReflectUtils.forName(soulValidation)).configure().buildValidatorFactory();
        } else {
            factory = Validation.buildDefaultValidatorFactory();
        }

        this.validator = factory.getValidator();
    }

    private static boolean isPrimitives(Class<?> cls) {
        return cls.isArray() ? isPrimitive(cls.getComponentType()) : isPrimitive(cls);
    }

    private static boolean isPrimitive(Class<?> cls) {
        return cls.isPrimitive() || cls == String.class || cls == Boolean.class || cls == Character.class || Number.class.isAssignableFrom(cls) || Date.class.isAssignableFrom(cls);
    }

    private static Object getMethodParameterBean(Class<?> clazz, Method method, Object[] args) {
        if (!hasConstraintParameter(method)) {
            return null;
        } else {
            try {
                String parameterClassName = generateMethodParameterClassName(clazz, method);

                Class parameterClass;
                try {
                    parameterClass = Class.forName(parameterClassName, true, clazz.getClassLoader());
                } catch (ClassNotFoundException var27) {
                    ClassPool pool = ClassGenerator.getClassPool(clazz.getClassLoader());
                    CtClass ctClass = pool.makeClass(parameterClassName);
                    ClassFile classFile = ctClass.getClassFile();
                    classFile.setVersionToJava5();
                    ctClass.addConstructor(CtNewConstructor.defaultConstructor(pool.getCtClass(parameterClassName)));
                    Class<?>[] parameterTypes = method.getParameterTypes();
                    Annotation[][] parameterAnnotations = method.getParameterAnnotations();

                    for(int i = 0; i < parameterTypes.length; ++i) {
                        Class<?> type = parameterTypes[i];
                        Annotation[] annotations = parameterAnnotations[i];
                        AnnotationsAttribute attribute = new AnnotationsAttribute(classFile.getConstPool(), "RuntimeVisibleAnnotations");
                        Annotation[] var15 = annotations;
                        int var16 = annotations.length;

                        for(int var17 = 0; var17 < var16; ++var17) {
                            Annotation annotation = var15[var17];
                            if (annotation.annotationType().isAnnotationPresent(Constraint.class)) {
                                javassist.bytecode.annotation.Annotation ja = new javassist.bytecode.annotation.Annotation(classFile.getConstPool(), pool.getCtClass(annotation.annotationType().getName()));
                                Method[] members = annotation.annotationType().getMethods();
                                Method[] var21 = members;
                                int var22 = members.length;

                                for(int var23 = 0; var23 < var22; ++var23) {
                                    Method member = var21[var23];
                                    if (Modifier.isPublic(member.getModifiers()) && member.getParameterTypes().length == 0 && member.getDeclaringClass() == annotation.annotationType()) {
                                        Object value = member.invoke(annotation);
                                        if (null != value) {
                                            MemberValue memberValue = createMemberValue(classFile.getConstPool(), pool.get(member.getReturnType().getName()), value);
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

                    parameterClass = ctClass.toClass(clazz.getClassLoader(), (ProtectionDomain)null);
                }

                Object parameterBean = parameterClass.newInstance();

                for(int i = 0; i < args.length; ++i) {
                    Field field = parameterClass.getField(method.getName() + "Argument" + i);
                    field.set(parameterBean, args[i]);
                }

                return parameterBean;
            } catch (Throwable var28) {
                logger.warn(var28.getMessage(), var28);
                return null;
            }
        }
    }

    private static String generateMethodParameterClassName(Class<?> clazz, Method method) {
        StringBuilder builder = (new StringBuilder()).append(clazz.getName()).append("_").append(toUpperMethoName(method.getName())).append("Parameter");
        Class<?>[] parameterTypes = method.getParameterTypes();
        Class[] var4 = parameterTypes;
        int var5 = parameterTypes.length;

        for(int var6 = 0; var6 < var5; ++var6) {
            Class<?> parameterType = var4[var6];
            builder.append("_").append(parameterType.getName());
        }

        return builder.toString();
    }

    private static boolean hasConstraintParameter(Method method) {
        Annotation[][] parameterAnnotations = method.getParameterAnnotations();
        if (parameterAnnotations != null && parameterAnnotations.length > 0) {
            Annotation[][] var2 = parameterAnnotations;
            int var3 = parameterAnnotations.length;

            for(int var4 = 0; var4 < var3; ++var4) {
                Annotation[] annotations = var2[var4];
                Annotation[] var6 = annotations;
                int var7 = annotations.length;

                for(int var8 = 0; var8 < var7; ++var8) {
                    Annotation annotation = var6[var8];
                    if (annotation.annotationType().isAnnotationPresent(Constraint.class)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    private static String toUpperMethoName(String methodName) {
        return methodName.substring(0, 1).toUpperCase() + methodName.substring(1);
    }

    private static MemberValue createMemberValue(ConstPool cp, CtClass type, Object value) throws NotFoundException {
        MemberValue memberValue = javassist.bytecode.annotation.Annotation.createMemberValue(cp, type);
        if (memberValue instanceof BooleanMemberValue) {
            ((BooleanMemberValue)memberValue).setValue((Boolean)value);
        } else if (memberValue instanceof ByteMemberValue) {
            ((ByteMemberValue)memberValue).setValue((Byte)value);
        } else if (memberValue instanceof CharMemberValue) {
            ((CharMemberValue)memberValue).setValue((Character)value);
        } else if (memberValue instanceof ShortMemberValue) {
            ((ShortMemberValue)memberValue).setValue((Short)value);
        } else if (memberValue instanceof IntegerMemberValue) {
            ((IntegerMemberValue)memberValue).setValue((Integer)value);
        } else if (memberValue instanceof LongMemberValue) {
            ((LongMemberValue)memberValue).setValue((Long)value);
        } else if (memberValue instanceof FloatMemberValue) {
            ((FloatMemberValue)memberValue).setValue((Float)value);
        } else if (memberValue instanceof DoubleMemberValue) {
            ((DoubleMemberValue)memberValue).setValue((Double)value);
        } else if (memberValue instanceof ClassMemberValue) {
            ((ClassMemberValue)memberValue).setValue(((Class)value).getName());
        } else if (memberValue instanceof StringMemberValue) {
            ((StringMemberValue)memberValue).setValue((String)value);
        } else if (memberValue instanceof EnumMemberValue) {
            ((EnumMemberValue)memberValue).setValue(((Enum)value).name());
        } else if (memberValue instanceof ArrayMemberValue) {
            CtClass arrayType = type.getComponentType();
            int len = Array.getLength(value);
            MemberValue[] members = new MemberValue[len];

            for(int i = 0; i < len; ++i) {
                members[i] = createMemberValue(cp, arrayType, Array.get(value, i));
            }

            ((ArrayMemberValue)memberValue).setValue(members);
        }

        return memberValue;
    }

    public void validate(String methodName, Class<?>[] parameterTypes, Object[] arguments) throws Exception {
        List<Class<?>> groups = new ArrayList();
        String methodClassName = this.clazz.getName() + "$" + toUpperMethoName(methodName);
        Class methodClass = null;

        try {
            methodClass = Class.forName(methodClassName, false, Thread.currentThread().getContextClassLoader());
            groups.add(methodClass);
        } catch (ClassNotFoundException var16) {
        }

        Set<ConstraintViolation<?>> violations = new HashSet();
        Method method = this.clazz.getMethod(methodName, parameterTypes);
        Class<?>[] methodClasses = null;
        if (method.isAnnotationPresent(MethodValidated.class)) {
            methodClasses = ((MethodValidated)method.getAnnotation(MethodValidated.class)).value();
            groups.addAll(Arrays.asList(methodClasses));
        }

        groups.add(0, Default.class);
        groups.add(1, this.clazz);
        Class<?>[] classgroups = (Class[])groups.toArray(new Class[0]);
        Object parameterBean = getMethodParameterBean(this.clazz, method, arguments);
        if (parameterBean != null) {
            violations.addAll(this.validator.validate(parameterBean, classgroups));
        }

        Object[] var12 = arguments;
        int var13 = arguments.length;

        for(int var14 = 0; var14 < var13; ++var14) {
            Object arg = var12[var14];
            this.validate((Set)violations, (Object)arg, (Class[])classgroups);
        }

        if (!violations.isEmpty()) {
            logger.error("Failed to validate service: " + this.clazz.getName() + ", method: " + methodName + ", cause: " + violations);
            StringBuilder validateError = new StringBuilder("");
            violations.stream().forEach(each -> validateError.append(each.getMessage()).append(","));
            throw new ValidationException(validateError.toString().substring(0, validateError.length() - 1));
        }
    }

    private void validate(Set<ConstraintViolation<?>> violations, Object arg, Class<?>... groups) {
        if (arg != null && !isPrimitives(arg.getClass())) {
            if (Object[].class.isInstance(arg)) {
                Object[] var4 = (Object[])((Object[])arg);
                int var5 = var4.length;

                for(int var6 = 0; var6 < var5; ++var6) {
                    Object item = var4[var6];
                    this.validate(violations, item, groups);
                }
            } else {
                Iterator var8;
                if (Collection.class.isInstance(arg)) {
                    var8 = ((Collection)arg).iterator();

                    while(var8.hasNext()) {
                        Object item = var8.next();
                        this.validate(violations, item, groups);
                    }
                } else if (Map.class.isInstance(arg)) {
                    var8 = ((Map)arg).entrySet().iterator();

                    while(var8.hasNext()) {
                        Map.Entry<?, ?> entry = (Map.Entry)var8.next();
                        this.validate(violations, entry.getKey(), groups);
                        this.validate(violations, entry.getValue(), groups);
                    }
                } else {
                    violations.addAll(this.validator.validate(arg, groups));
                }
            }
        }

    }
}
