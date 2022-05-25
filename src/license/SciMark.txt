
        SciMark 2.0  Java Numerical Benchmark

        Roldan Pozo, Bruce Miller

        NIST

SciMark 2.0 is a composite Java benchmark measuring the  performance of 
numerical kernels occurring in scientific and engineering applications.  
It consists of five kernels which typify computational routines
commonly found in numeric codes: Fast Fourier Transforms (FFTs), 
Jacobi Successive Over-relaxation (SOR), Sparse matrix-multiply, 
Monte Carlo integration, and dense LU matrix factorization.

(See http://www.math.nist.gov/scimark for further information
and latest updates.)


1) INSTALLATION

Unpack the contents of archive into a subdirectory on your
CLASSPATH.  Be sure to keep the directory structure of the
file contents.

2) COMPILING THE BENCHMARKS (optional)

From the directory above this one, issue the command:

    >javac -O commandline.java

This should compile main benchmark driver and dependent files.

3) RUNNING THE BENCHMARKS

From the directory above this one, issue the command:

    >java jnt.scimark2.commandline

or
    >java jnt.scimark2.commandline -large

to run the large problem size version. (Note that this one
takes considerably longer to run.)

After a few minutes, the program should respond with
the benchmark results, e.g.

    >javac jnt.scimark2.commandline

	SciMark 2.0a

    Composite Score: 20.791595999749727
    FFT (4096): 30.260047144878346
    Jacobi SOR (100x100):   33.074935359763934
    Monte Carlo (25000): 11.510791361970528
    Sparse matmult (nz=25000), 10 iterations: 8.007507030681996
    LU (100x100): 21.104699101453836

    java.vendor: Sun Microsystems Inc.
    java.version: 1.2
    os.arch: x86
    os.name: Windows NT
    os.version: 4.0

One can send these results to "pozo@nist.gov".  

/* ----------------------  END OF README -----------------------------*/
