package org.diffax;

/** Lightweight immutable complex number used by the DIFFaX numerical kernel. */
public record Complex(double re, double im) {
    public static final Complex ZERO = new Complex(0.0, 0.0);
    public static final Complex ONE  = new Complex(1.0, 0.0);

    public Complex add(Complex z) { return new Complex(re + z.re, im + z.im); }
    public Complex sub(Complex z) { return new Complex(re - z.re, im - z.im); }
    public Complex mul(Complex z) {
        return new Complex(re * z.re - im * z.im, re * z.im + im * z.re);
    }
    public Complex mul(double x) { return new Complex(re * x, im * x); }
    public Complex div(Complex z) {
        double d = z.re * z.re + z.im * z.im;
        return new Complex((re * z.re + im * z.im) / d,
                           (im * z.re - re * z.im) / d);
    }
    public Complex conj() { return new Complex(re, -im); }
    public double abs() { return Math.hypot(re, im); }
    public double abs1() { return Math.abs(re) + Math.abs(im); }
    public static Complex expi(double x) { return new Complex(Math.cos(x), Math.sin(x)); }
}
