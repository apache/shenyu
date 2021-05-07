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

package org.apache.shenyu.plugin.grpc.resolver;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.protobuf.DescriptorProtos.FileDescriptorProto;
import com.google.protobuf.DescriptorProtos.FileDescriptorSet;
import com.google.protobuf.Descriptors;
import com.google.protobuf.Descriptors.Descriptor;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.MetaData;

import java.util.HashMap;
import java.util.Map;

/**
 * Read proto file descriptors and extract method definitions.
 *
 * @author zhanglei
 */
@Slf4j
public final class ServiceResolver {

    private final ImmutableList<Descriptors.FileDescriptor> fileDescriptors;

    private ServiceResolver(final Iterable<Descriptors.FileDescriptor> fileDescriptors) {
        this.fileDescriptors = ImmutableList.copyOf(fileDescriptors);
    }

    /**
     * Creates a resolver.
     *
     * @param descriptorSet descriptorSet
     * @return ServiceResolver serviceResolver
     */
    public static ServiceResolver fromFileDescriptorSet(final FileDescriptorSet descriptorSet) {
        ImmutableMap<String, FileDescriptorProto> descriptorProtoIndex =
                computeDescriptorProtoIndex(descriptorSet);
        Map<String, Descriptors.FileDescriptor> descriptorCache = new HashMap<>();

        ImmutableList.Builder<Descriptors.FileDescriptor> result = ImmutableList.builder();
        for (FileDescriptorProto descriptorProto : descriptorSet.getFileList()) {
            try {
                result.add(descriptorFromProto(descriptorProto, descriptorProtoIndex, descriptorCache));
            } catch (Descriptors.DescriptorValidationException e) {
                log.warn("Skipped descriptor " + descriptorProto.getName() + " due to error", e);
            }
        }
        return new ServiceResolver(result.build());
    }

    /**
     * Lists all the known message types.
     *
     * @return ImmutableSet set
     */
    public ImmutableSet<Descriptor> listMessageTypes() {
        ImmutableSet.Builder<Descriptor> resultBuilder = ImmutableSet.builder();
        fileDescriptors.forEach(d -> resultBuilder.addAll(d.getMessageTypes()));
        return resultBuilder.build();
    }

    /**
     * Resolve service method.
     *
     * @param metaData metaData
     * @return MethodDescriptor
     */
    public Descriptors.MethodDescriptor resolveServiceMethod(final MetaData metaData) {
        String fullServiceName = metaData.getServiceName();
        String serviceName = fullServiceName.substring(fullServiceName.lastIndexOf(".") + 1);
        String packageName = fullServiceName.substring(0, fullServiceName.lastIndexOf("."));
        Descriptors.ServiceDescriptor service = findService(packageName, serviceName);
        Descriptors.MethodDescriptor method = service.findMethodByName(metaData.getMethodName());
        if (method == null) {
            throw new IllegalArgumentException("Unable to find method " + packageName + " in service " + serviceName);
        }
        return method;
    }

    private Descriptors.ServiceDescriptor findService(final String packageName, final String serviceName) {
        for (Descriptors.FileDescriptor fileDescriptor : fileDescriptors) {
            if (!fileDescriptor.getPackage().equals(packageName)) {
                continue;
            }
            Descriptors.ServiceDescriptor serviceDescriptor = fileDescriptor.findServiceByName(serviceName);
            if (serviceDescriptor != null) {
                return serviceDescriptor;
            }
        }
        throw new IllegalArgumentException("Unable to find service with name: " + serviceName);
    }

    private static ImmutableMap<String, FileDescriptorProto> computeDescriptorProtoIndex(final FileDescriptorSet fileDescriptorSet) {
        ImmutableMap.Builder<String, FileDescriptorProto> resultBuilder = ImmutableMap.builder();
        for (FileDescriptorProto descriptorProto : fileDescriptorSet.getFileList()) {
            resultBuilder.put(descriptorProto.getName(), descriptorProto);
        }
        return resultBuilder.build();
    }

    private static Descriptors.FileDescriptor descriptorFromProto(
            final FileDescriptorProto descriptorProto,
            final ImmutableMap<String, FileDescriptorProto> descriptorProtoIndex,
            final Map<String, Descriptors.FileDescriptor> descriptorCache) throws Descriptors.DescriptorValidationException {
        // First check the cache.
        String descriptorName = descriptorProto.getName();
        if (descriptorCache.containsKey(descriptorName)) {
            return descriptorCache.get(descriptorName);
        }
        // Then fetch all the required dependencies recursively.
        ImmutableList.Builder<Descriptors.FileDescriptor> dependencies = ImmutableList.builder();
        for (String dependencyName : descriptorProto.getDependencyList()) {
            if (!descriptorProtoIndex.containsKey(dependencyName)) {
                throw new IllegalArgumentException("Could not find dependency: " + dependencyName);
            }
            FileDescriptorProto dependencyProto = descriptorProtoIndex.get(dependencyName);
            dependencies.add(descriptorFromProto(dependencyProto, descriptorProtoIndex, descriptorCache));
        }
        // Finally construct the actual descriptor.
        Descriptors.FileDescriptor[] empty = new Descriptors.FileDescriptor[0];
        return Descriptors.FileDescriptor.buildFrom(descriptorProto, dependencies.build().toArray(empty));
    }
}
