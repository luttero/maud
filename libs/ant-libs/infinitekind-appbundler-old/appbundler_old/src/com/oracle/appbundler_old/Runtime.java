/*
 * Copyright 2022, oracle and/or its affiliates. All rights reserved.
 *
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact oracle
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package com.oracle.appbundler_old;

import java.io.File;
import java.io.IOException;

import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.types.FileSet;

public class Runtime extends FileSet {

    /* Override to provide canonical path so that runtime can be specified
     * via a version-agnostic path (relative link, e.g. `current-jre`) while
     * still preserving the original runtime directory name, e.g. `jre1.8.0_45.jre`.
     */
    @Override
    public File getDir() {
        File dir = super.getDir();
        try {
            return dir.getCanonicalFile();
        } catch (IOException e) {
            return dir;
        }
    }

    private void detectType() {
            appendIncludes(new String[] {
                    "bin/",
                    "conf/",
                    "include/",
                    "jmods/",
                    "legal/",
                    "lib/",
                    "release"
            });
    }

    void copyTo(File targetDir) throws IOException {
        detectType();
        
        File runtimeHomeDirectory = getDir();
        File runtimeContentsDirectory = runtimeHomeDirectory.getParentFile();
        File runtimeDirectory = runtimeContentsDirectory.getParentFile();
        
        // Create root plug-in directory
        File pluginDirectory = new File(targetDir, runtimeHomeDirectory.getName());
        pluginDirectory.mkdir();

        DirectoryScanner directoryScanner = getDirectoryScanner(getProject());
        String[] includedFiles = directoryScanner.getIncludedFiles();

        for (int i = 0; i < includedFiles.length; i++) {
            String includedFile = includedFiles[i];
            File source = new File(runtimeHomeDirectory, includedFile);
            File destination = new File(pluginDirectory, includedFile);
            AppBundlerTask.copy(source, destination);
        }
    }
}
