package it.unitn.ing.rista.diffr.rsa.epsc;

/**
 * A simple mutable wrapper class to allow methods to
 * "return" an integer by modifying its 'value' field.
 * This is used to mimic Fortran's pass-by-reference for scalars.
 */
public class IntHolder {
    public int value;

    /**
     * Constructs a new IntHolder with an initial value.
     * @param value The initial integer value.
     */
    public IntHolder(int value) {
        this.value = value;
    }
}
