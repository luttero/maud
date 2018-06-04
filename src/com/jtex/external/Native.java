/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.external;

import com.jtex.util.Utilities;
import it.unitn.ing.rista.util.Constants;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author hios
 */
public class Native {

    public static final String FC2ODF = "fc2odf";
    public static final String ODF2FC = "odf2fc";
    public static final String ODF2PF = "odf2pf";
    public static final String PDF2PF = "pdf2pf";
    public static final String PF2ODF = "pf2odf";

    private static final String EXECUTABLE_JARPATH = "/com/jtex/external/bin/" + os() + arch() + "/";
    private static final String EXECUTABLE_PATH = execpath();

    static String os() {
        String osName = System.getProperty("os.name");
        if (osName.equals("Linux")) {
            return "glnx";
        }
        if (osName.equals("Mac OS X")) {
            return "maci";
        }
        if (osName.startsWith("Win")) {
            return "win";
        }
        return osName;
    }

    static String arch() {
        String osArch = System.getProperty("os.arch");
        if (osArch.equals("i386") || osArch.equals("i686")) {
            return "86";
        }
        if (osArch.equals("amd64") || osArch.equals("IA64N") || osArch.equals("x86_64") || osArch.equals("IA64W")) {
            return "64";
        }
        return osArch;
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        loadLibraries();

        ParamSet set = new ParamSet(FC2ODF, 2);

        set.add("res1", new double[]{1, 5, 235, 11, 4246, 2, 3, 4, 5});
        set.addinline("lP", new double[]{1, 2, 3, 4});
        set.addinline("lP", new int[]{1, 2, 3, 4});

        execute(set);

    }

    public static void loadLibraries() {
        String[] libnames = {FC2ODF, ODF2FC, ODF2PF, PDF2PF, PF2ODF};
        for (String lib : libnames) {
            loadLibrary(lib);
        }
    }

    public static void loadLibrary(String libname) {
/*        try {
            String lib = libname + (os().startsWith("win") ? ".exe" : "");

            File f = new File(EXECUTABLE_PATH);
            if (!f.exists() || !f.isDirectory()) {
                f.mkdir();
            }

            File target = new File(f + File.separator + lib);
            if (!target.isFile()) {
//            	System.out.println("Loading: " + EXECUTABLE_JARPATH + lib);
                InputStream in = Native.class.getResourceAsStream(EXECUTABLE_JARPATH + lib);
                File ex = File.createTempFile(libname, "", f);
                FileOutputStream fos = new FileOutputStream(ex);
                int len;
                byte[] buf = new byte[4096];
                while ((len = in.read(buf)) != -1) {
                    fos.write(buf, 0, len);
                }
                fos.close();
                in.close();
                ex.renameTo(target);
            }
	        Utilities.setPermission(target, true);
//	        target.setExecutable(true);
        } catch (IOException ex) {
            Logger.getLogger(Native.class.getName()).log(Level.SEVERE, null, ex);
        }*/
    }

    public static double[] execute(ParamSet set) {
        double[] result = null;
        set.close();

        try {
            List<String> cmd = new ArrayList<String>();
	         cmd.add(EXECUTABLE_PATH + set.getExec());
            cmd.add(set.getParamFile().getAbsolutePath());

            System.out.println(cmd);
            ProcessBuilder p = new ProcessBuilder(cmd);
            System.out.println(p.toString());
            Process process = p.start();
            int i = process.waitFor();
            if (i == 0) {
                result = set.read(0);
            }
        } catch (IOException ex) {
            Logger.getLogger(Native.class.getName()).log(Level.SEVERE, "Mtex: IO problem running " + set.getExec(), ex);
//            System.exit(0);
        } catch (InterruptedException ex) {
            Logger.getLogger(Native.class.getName()).log(Level.SEVERE, "Mtex: " + set.getExec() + " not running properly!", ex);
        }

        set.cleanup();
        return result;
    }

    private static String execpath() {
        String tmpdir = Utilities.getAppPath() + "mtex" + File.separator;
        File path = new File(tmpdir);
        if (!path.exists()) {
            path.mkdirs();
        }
        return tmpdir;
    }

    public static class ParamSet {

        File tempPath = getTempPath();
        String exec;
        String prefix;
        File paramFile, res;
        Formatter paramFormatter;
        List<File> tempFiles;
        int nout;

        private static File getTempPath() {
            File path = new File(Constants.cachesDirectory + File.separator + "tmp");
            if (!path.exists()) {
                path.mkdirs();
            }
            return path;
        }

        public ParamSet(String exec, int nout) {
            this.exec = exec;
            this.nout = nout;
            try {
//            	System.out.println("Creating file: " + exec + " " + tempPath);
                this.paramFile = File.createTempFile(exec, ".txt", tempPath);
                this.paramFile.createNewFile();
                this.paramFormatter = new Formatter(paramFile, "UTF-8", Locale.US);

                this.prefix = this.paramFile.getAbsolutePath();
                this.prefix = this.prefix.substring(0, this.prefix.lastIndexOf("."));
            } catch (IOException ex) {
                Logger.getLogger(Native.class.getName()).log(Level.SEVERE, null, ex);
            }
            this.tempFiles = new ArrayList<File>();

            for (int i = 1; i <= nout; i++) {
                add("res" + i, null);
            }
//            System.out.println(this.paramFile);
        }

        public String getExec() {
            return exec;
        }

        public File getParamFile() {
            return paramFile;
        }

        public void add(String name, Object val) {
            File tmpparam = new File(prefix + "_" + name + ".dat");
            tempFiles.add(tmpparam);
            paramFormatter.format("%s: %s\r\n", name, tmpparam.getAbsoluteFile());
            paramFormatter.flush();

//            System.out.println("Mtex: saving parameters for running to: " + tmpparam.getAbsoluteFile());
            try {
                RandomAccessFile tmpfile = new RandomAccessFile(tmpparam, "rw");
                if (val instanceof double[]) {
                    double[] d = (double[]) val;
                    ByteBuffer b2 = ByteBuffer.allocate(d.length * 8);
                    b2.order(ByteOrder.nativeOrder());
                    b2.asDoubleBuffer().put(d);
                    tmpfile.write(b2.array());
                } else if (val instanceof int[]) {
                    int[] d = (int[]) val;
                    ByteBuffer b2 = ByteBuffer.allocate(d.length * 4);
                    b2.order(ByteOrder.nativeOrder());
                    b2.asIntBuffer().put(d);
                    tmpfile.write(b2.array());
                }
	            tmpfile.close();
					Utilities.setPermission(tmpparam, false);
					if (name.startsWith("res"))
						tmpparam.delete();
            } catch (FileNotFoundException ex) {
                Logger.getLogger(Native.class.getName()).log(Level.SEVERE,
		                "Mtex: add parameter and save, file not found: " + tmpparam.getAbsolutePath(), ex);
            } catch (IOException ex) {
                Logger.getLogger(Native.class.getName()).log(Level.SEVERE, "Mtex: add parameter and save, IO exception: " + tmpparam.getAbsolutePath(), ex);
            }

        }

        public void addinline(String name, double... val) {
            paramFormatter.format("%s: ", name);
            for (int i = 0; i < val.length; i++) {
                paramFormatter.format(" %E", val[i]);
            }
            paramFormatter.format("\r\n");
            paramFormatter.flush();
        }

        public void addinline(String name, int... val) {
            paramFormatter.format("%s: ", name);
            for (int i = 0; i < val.length; i++) {
                paramFormatter.format(" %d", val[i]);
            }
            paramFormatter.format("\r\n");
            paramFormatter.flush();
        }

        public double[] read(int i) {
            try {
                RandomAccessFile fa = new RandomAccessFile(tempFiles.get(i), "r");
                FileChannel inChannel = fa.getChannel();
                long fileSize = inChannel.size();
                ByteBuffer buffer = ByteBuffer.allocate((int) fileSize);
                inChannel.read(buffer);
                buffer.rewind();
                fa.close();

                double[] out = new double[(int) fileSize / 8];
                buffer.order(ByteOrder.nativeOrder()).asDoubleBuffer().get(out);
                return out;
            } catch (FileNotFoundException ex) {
                Logger.getLogger(Native.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IOException ex) {
                Logger.getLogger(Native.class.getName()).log(Level.SEVERE, null, ex);
            }
            return null;
        }

        private void close() {
            paramFormatter.close();
        }

        private void cleanup() {
/*            paramFile.delete();

            for (File f : tempFiles) {
                f.delete();
            }*/

        }

    }

}
