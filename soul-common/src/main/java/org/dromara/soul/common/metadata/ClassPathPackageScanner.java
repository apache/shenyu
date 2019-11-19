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

import java.io.File;
import java.io.IOException;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.exception.SoulException;

/**
 * ClassPathPackageScanner.
 *
 * @author sixh
 */
final class ClassPathPackageScanner implements PackageScanner {

    private String basePackage;

    private ClassLoader cl;

    public ClassPathPackageScanner(String basePackage) {
        this.basePackage = basePackage;
        this.cl = this.getClass().getClassLoader();
    }

    public ClassPathPackageScanner(String basePackage, ClassLoader cl) {
        this.basePackage = basePackage;
        this.cl = cl;
    }

    @Override
    public Set<Resource> getFullClass() throws IOException {
        Set<Resource> allNames = new HashSet<>();
        doScan(basePackage, allNames);
        return allNames;
    }

    private void doScan(String basePackage, Set<Resource> allNames) throws IOException {
        String splash = PackageScanner.toSplash(basePackage);
        Enumeration<URL> resources = this.cl.getResources(splash);
        Set<Resource> result = new LinkedHashSet<>(16);
        while (resources.hasMoreElements()) {
            URL url = resources.nextElement();
            result.add(new UrlResource(url));
        }
        for (Resource resource : result) {
            Set<Resource> names;
            URL rootDir = resource.getURL();
            if (isJar(resource.getURL())) {
                names = readFromJarFile(resource, rootDir);
            } else {
                String rootPath = PackageScanner.getRootPath(rootDir);
                names = readFromDirectory(rootPath);
            }
            allNames.addAll(names);
        }
    }

    private Set<Resource> readFromDirectory(String path) throws IOException {
        File file = new File(path);
        if (!file.exists()) {
            return Collections.emptySet();
        }
        if (!file.isDirectory()) {
            return Collections.emptySet();
        }
        if (!file.canRead()) {
            return Collections.emptySet();
        }
        Set<File> files = new HashSet<>();
        readFromDirectory(file, files);
        Set<Resource> fileResource = new HashSet<>();
        for (File result : files) {
            fileResource.add(new FileResource(result));
        }
        return fileResource;
    }

    private void readFromDirectory(File dir, Set<File> result) {
        for (File content : listDirectory(dir)) {
            String currPath = StringUtils.replace(content.getAbsolutePath(), File.separator, "/");
            if (content.isDirectory()) {
                if (content.canRead()) {
                    readFromDirectory(content, result);
                }
            }
            if (isClass(currPath)) {
                result.add(content);
            }
        }
    }

    private File[] listDirectory(File dir) {
        File[] files = dir.listFiles();
        if (files == null) {
            return new File[0];
        }
        Arrays.sort(files, Comparator.comparing(File::getName));
        return files;
    }

    private Set<Resource> readFromJarFile(Resource rootDirResource, URL rootDir) throws IOException {
        URLConnection con = rootDir.openConnection();
        JarFile jarFile;
        String rootEntryPath;
        boolean closeJarFile;

        if (con instanceof JarURLConnection) {
            JarURLConnection jarCon = (JarURLConnection) con;
            jarFile = jarCon.getJarFile();
            JarEntry jarEntry = jarCon.getJarEntry();
            rootEntryPath = (jarEntry != null ? jarEntry.getName() : "");
            closeJarFile = !jarCon.getUseCaches();
        } else {
            throw new SoulException("file " + rootDir + " is not jar");
        }
        try {
            if (!"".equals(rootEntryPath) && !rootEntryPath.endsWith("/")) {
                rootEntryPath = rootEntryPath + "/";
            }
            Set<Resource> result = new LinkedHashSet<>(8);
            for (Enumeration<JarEntry> entries = jarFile.entries(); entries.hasMoreElements(); ) {
                JarEntry entry = entries.nextElement();
                String entryPath = entry.getName();
                if (entryPath.startsWith(rootEntryPath)) {
                    String relativePath = entryPath.substring(rootEntryPath.length());
                    if (isClass(relativePath)) {
                        result.add(rootDirResource.createRelative(relativePath));
                    }
                }
            }
            return result;
        } finally {
            if (closeJarFile) {
                jarFile.close();
            }
        }
    }

    private boolean isJar(URL url) {
        String protocol = url.getProtocol();
        String jarFile = "jar";
        return protocol.equals(jarFile);
    }

    private boolean isClass(String relativePath) {
        return relativePath.endsWith(".class");
    }

}
