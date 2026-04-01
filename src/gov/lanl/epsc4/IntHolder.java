package gov.lanl.epsc4;

/**
 * A simple mutable wrapper class to allow methods to
 * "return" an integer by modifying its 'value' field.
 * This is used to mimic Fortran's pass-by-reference for scalars.
 */
public class IntHolder {
    public int value;
    public IntHolder(int value) { this.value = value; }
}
