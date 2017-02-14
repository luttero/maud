package it.unitn.ing.rista.mdyn;

import java.lang.*;


/** Implements a ring buffer
 *
 * @version 1.0 (10-Oct-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class RingBuffer
        extends Object {


  // default ring buffer size
  final static private int DEFAULTSIZE = 10;

  // buffer index pointers
  private int idxPut;
  private int idxGet;

  // actual buffer size
  private int size;

  // buffer array
  private Object[] buf = new Object[DEFAULTSIZE];


  /** Create a ring buffer object
   */

  public RingBuffer() {

    reset();

  } // end RingBuffer


  /** Create a ring buffer object with custom size
   */

  public RingBuffer(int size) {

    this();
    buf = new Object[size];

  } // end RingBuffer


  /** Resets the ring buffer (clears all contents)
   */

  public void reset() {

    idxPut = 0;
    idxGet = 0;
    size = 0;

  } // end reset


  /** Put an object into the ring buffer
   * @param data data object
   */

  public void put(Object data) {

    // store data
    buf[idxPut] = data;

    // increase index pointer
    idxPut = (idxPut + 1) % buf.length;

    // in case of buffer overrun, skip last data object
    if (idxPut == idxGet) {
      idxGet = (idxGet + 1) % buf.length;
    } else {
      size++;
    }

  } // end put


  /** Get an object from the ring buffer
   * @return data object (or null if buffer underrun)
   */

  public Object get() {

    // in case of buffer underrun return null
    if (idxGet == idxPut) {
      return (null);
    }

    // retrieve data
    Object data = buf[idxGet];

    // increase index pointer
    idxGet = (idxGet + 1) % buf.length;
    size--;

    // return data object
    return (data);

  } // end get


  /** Peeks ahead n objects
   * @param n number of objects to peek ahead
   * @return data object
   */

  public Object peek(int n) {

    // in case of buffer underrun return null
    if (n >= size()) {
      return (null);
    }

    // return data object
    return (buf[(idxGet + n) % buf.length]);

  } // end peek


  /** Gets the actual size of the buffer
   * @return buffer size
   */

  public int size() {

    return (size);

  } // end size


  /** Creates a string representation of this object
   * @return string representation of this object
   */

  public String toString() {

    return ("Cap: " + buf.length + " PutIdx: " + idxPut + " GetIdx: " + idxGet);

  } // end toXRDcatString


} // end RingBuffer
