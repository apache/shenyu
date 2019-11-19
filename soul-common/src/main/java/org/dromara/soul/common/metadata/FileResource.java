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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

/**
 * FileResource
 *
 * @author sixh
 */
final class FileResource implements Resource {

    private File file;

    private final String path;

    private final Path filePath;

    private static final String FOLDER_SEPARATOR = "/";

    private FileResource(String path) {
        this(new File(path));
    }


    FileResource(File file) {
        this.file = file;
        this.path = file.getPath();
        this.filePath = file.toPath();
    }

    private FileResource(FileSystem fileSystem, String pathToUse) {
        this.path = pathToUse;
        this.file = null;
        this.filePath = fileSystem.getPath(this.path).normalize();
    }

    @Override
    public Resource createRelative(String relativePath) throws IOException {
        String pathToUse = applyRelativePath(this.path, relativePath);
        return (this.file != null ? new FileResource(pathToUse) :
                new FileResource(this.filePath.getFileSystem(), pathToUse));
    }

    private static String applyRelativePath(String path, String relativePath) {
        int separatorIndex = path.lastIndexOf(FOLDER_SEPARATOR);
        if (separatorIndex != -1) {
            String newPath = path.substring(0, separatorIndex);
            if (!relativePath.startsWith(FOLDER_SEPARATOR)) {
                newPath += FOLDER_SEPARATOR;
            }
            return newPath + relativePath;
        } else {
            return relativePath;
        }
    }

    @Override
    public URL getURL() throws MalformedURLException {
        return (this.file != null ? this.file.toURI().toURL() : this.filePath.toUri().toURL());
    }

    @Override
    public String getFileName() {
        return path;
    }

    @Override
    public InputStream getInputStream() throws IOException {
        try {
            return Files.newInputStream(this.filePath);
        } catch (NoSuchFileException ex) {
            throw new FileNotFoundException(ex.getMessage());
        }
    }

    @Override
    public boolean equals(Object other) {
        return (this == other || (other instanceof FileResource &&
                                  this.path.equals(((FileResource) other).path)));
    }

    /**
     * This implementation returns the hash code of the underlying File reference.
     */
    @Override
    public int hashCode() {
        return this.path.hashCode();
    }

}
