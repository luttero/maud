/* jpvmBuffer.java
 *
 * Classes to implement a packable / unpackable buffer that can
 * be used to store the payload associated with a message.
 *
 * Adam J Ferrari
 * Sun 05-26-1996
 *
 * modified by
 * Luca Lutterotti
 * Verona, 12 November 1999
 *
 * Copyright (C) 1996  Adam J Ferrari, 1999 Luca Lutterotti
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

package it.unitn.ing.rista.jpvm;

import java.io.*;

/**
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2004/01/21 11:14:51 $
 * @since JDK1.1
 */

public class jpvmBuffer {
  private jpvmBufferElement list_head;
  private jpvmBufferElement list_tail;
  private jpvmBufferElement curr_elt;
  private int num_list_elts;

  private void addElt(jpvmBufferElement nw) {
    num_list_elts++;
    if (list_head == null) {
      curr_elt = list_head = list_tail = nw;
      return;
    }
    list_tail.next = nw;
    list_tail = nw;
  }

  public jpvmBuffer() {
    list_head = null;
    list_tail = null;
    num_list_elts = 0;
  }

  public void rewind() {
    curr_elt = list_head;
  }

  public void pack(int d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(int d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(char d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(char d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(short d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(short d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(long d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(long d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(byte d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(byte d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(float d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(float d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(double d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(double d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(jpvmTaskId d[], int n, int stride) {
    jpvmBufferElement nw = new jpvmBufferElement(d, n, stride);
    addElt(nw);
  }

  public void pack(jpvmTaskId d) {
    jpvmBufferElement nw = new jpvmBufferElement(d);
    addElt(nw);
  }

  public void pack(String str) {
    jpvmBufferElement nw = new jpvmBufferElement(str);
    addElt(nw);
  }

  public void unpack(int d[], int n, int stride) throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upkint.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public int upkint() throws jpvmException {
    int d[] = new int[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public void unpack(byte d[], int n, int stride) throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upkbyte.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public byte upkbyte() throws jpvmException {
    byte d[] = new byte[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public void unpack(char d[], int n, int stride) throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upkchar.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public char upkchar() throws jpvmException {
    char d[] = new char[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public void unpack(short d[], int n, int stride) throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upkshort.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public short upkshort() throws jpvmException {
    short d[] = new short[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public void unpack(long d[], int n, int stride) throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upklong.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public long upklong() throws jpvmException {
    long d[] = new long[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public void unpack(float d[], int n, int stride) throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upkfloat.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public float upkfloat() throws jpvmException {
    float d[] = new float[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public void unpack(double d[], int n, int stride) throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upkdouble.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public double upkdouble() throws jpvmException {
    double d[] = new double[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public void unpack(jpvmTaskId d[], int n, int stride)
      throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upktid.");
    curr_elt.unpack(d, n, stride);
    curr_elt = curr_elt.next;
  }

  public jpvmTaskId upktid() throws jpvmException {
    jpvmTaskId d[] = new jpvmTaskId[1];
    unpack(d, 1, 1);
    return d[0];
  }

  public String upkstr() throws jpvmException {
    if (curr_elt == null)
      throw new jpvmException("buffer empty, upkstring.");
    String ret = curr_elt.unpack();
    curr_elt = curr_elt.next;
    return ret;
  }

  public void send(jpvmSendConnection conn) throws jpvmException {
    DataOutputStream strm = conn.strm;
    jpvmBufferElement tmp = list_head;
    try {
      strm.writeInt(num_list_elts);
      while (tmp != null) {
        tmp.send(conn);
        tmp = tmp.next;
      }
    } catch (IOException ioe) {
      jpvmDebug.note("jpvmBuffer, send - i/o exception");
      throw new jpvmException("jpvmBuffer, send - " +
          "i/o exception");
    }
  }

  public void recv(jpvmRecvConnection conn) throws jpvmException {
    int i, N;
    jpvmBufferElement tmp;

    DataInputStream strm = conn.strm;
    try {
      N = strm.readInt();
      jpvmDebug.note("jpvmBuffer, recv " + N +
          " buffer elements.");
      for (i = 0; i < N; i++) {
        tmp = new jpvmBufferElement();
        tmp.recv(conn);
        addElt(tmp);
      }
    } catch (IOException ioe) {
      jpvmDebug.note("jpvmBuffer, recv - i/o exception");
      throw new jpvmException("jpvmBuffer, recv - " +
          "i/o exception");
    }
  }
}

class jpvmBufferElement {
  private boolean inPlace;
  public jpvmBufferElementContents contents;
  public jpvmBufferElement next; // Linked structure

  public void init() {
    contents = null;
    next = null;
  }

  public jpvmBufferElement() {
    init();
    inPlace = false;
  }

  public jpvmBufferElement(boolean dataInPlace) {
    init();
    inPlace = dataInPlace;
  }

  public jpvmBufferElement(jpvmTaskId d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(jpvmTaskId d) {
    init();
    jpvmTaskId a[] = new jpvmTaskId[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }

  public jpvmBufferElement(byte d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(byte d) {
    init();
    byte a[] = new byte[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }

  public jpvmBufferElement(short d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(short d) {
    init();
    short a[] = new short[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }

  public jpvmBufferElement(char d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(char d) {
    init();
    char a[] = new char[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }


  public jpvmBufferElement(long d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(long d) {
    init();
    long a[] = new long[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }


  public jpvmBufferElement(int d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(int d) {
    init();
    int a[] = new int[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }

  public jpvmBufferElement(float d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(float d) {
    init();
    float a[] = new float[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }

  public jpvmBufferElement(double d[], int n, int stride) {
    init();
    contents = new jpvmBufferElementContents(d, n, stride, inPlace);
  }

  public jpvmBufferElement(double d) {
    init();
    double a[] = new double[1];
    a[0] = d;
    contents = new jpvmBufferElementContents(a, 1, 1, true);
  }

  public jpvmBufferElement(String str) {
    init();
    int n = str.length();
    char a[] = new char[n];
    str.getChars(0, n, a, 0);
    contents = new jpvmBufferElementContents(a, n, 1, true);
    contents.dataType = jpvmDataType.jpvmString;
  }

  public void unpack(int d[], int n, int stride) throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public void unpack(short d[], int n, int stride) throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public void unpack(byte d[], int n, int stride) throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public void unpack(char d[], int n, int stride) throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public void unpack(long d[], int n, int stride) throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public void unpack(double d[], int n, int stride) throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public void unpack(float d[], int n, int stride) throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public void unpack(jpvmTaskId d[], int n, int stride)
      throws jpvmException {
    contents.unpack(d, n, stride);
  }

  public String unpack() throws jpvmException {
    return contents.unpack();
  }

  public void send(jpvmSendConnection conn) throws jpvmException {
    try {
      ObjectOutputStream out;
      out = new ObjectOutputStream(conn.strm);
      out.writeObject(contents);
      //out.flush();
    } catch (IOException ioe) {
      System.err.println("I/O exception - " + ioe);
      ioe.printStackTrace();
      jpvmDebug.note("jpvmBufferElement, " +
          "send - i/o exception");
      throw new jpvmException("jpvmBufferElement, " +
          "send - i/o exception");
    }
  }

  public void recv(jpvmRecvConnection conn) throws jpvmException {
    try {
      ObjectInputStream in;
      in = new ObjectInputStream(conn.strm);
      try {
        contents = (jpvmBufferElementContents) in.readObject();
      } catch (ClassNotFoundException cnf) {
        throw new jpvmException("jpvmBufferElement, " +
            "recv - can't find class " +
            "jpvmBufferElementContents");
      }
    } catch (IOException ioe) {
      ioe.printStackTrace();
      jpvmDebug.note("jpvmBufferElement, " +
          "recv - i/o exception");
      throw new jpvmException("jpvmBufferElement, " +
          "recv - i/o exception");
    }
  }
}


class jpvmBufferElementContents implements Serializable {
  public int dataType;
  public int arraySize;
  public byte byteArray[];
  public char charArray[];
  public int intArray[];
  public short shortArray[];
  public long longArray[];
  public float floatArray[];
  public double doubleArray[];
  public jpvmTaskId taskArray[];

  private void init() {
    dataType = jpvmDataType.jpvmNull;
    arraySize = 0;
    byteArray = null;
    charArray = null;
    shortArray = null;
    intArray = null;
    longArray = null;
    floatArray = null;
    doubleArray = null;
    taskArray = null;
  }

  public jpvmBufferElementContents(jpvmTaskId d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmTid;
    if (stride == 1) {
      if (inPlace)
        taskArray = d;
      else {
        taskArray = new jpvmTaskId[n];
        System.arraycopy(d, 0, taskArray, 0, n);
      }
    } else {
      taskArray = new jpvmTaskId[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        taskArray[i] = d[j];
    }
  }

  public jpvmBufferElementContents(short d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmShort;
    if (stride == 1) {
      if (inPlace)
        shortArray = d;
      else {
        shortArray = new short[n];
        System.arraycopy(d, 0, shortArray, 0, n);
      }
    } else {
      shortArray = new short[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        shortArray[i] = d[j];
    }
  }

  public jpvmBufferElementContents(int d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmInteger;
    if (stride == 1) {
      if (inPlace)
        intArray = d;
      else {
        intArray = new int[n];
        System.arraycopy(d, 0, intArray, 0, n);
      }
    } else {
      intArray = new int[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        intArray[i] = d[j];
    }
  }

  public jpvmBufferElementContents(long d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmLong;
    if (stride == 1) {
      if (inPlace)
        longArray = d;
      else {
        longArray = new long[n];
        System.arraycopy(d, 0, longArray, 0, n);
      }
    } else {
      longArray = new long[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        longArray[i] = d[j];
    }
  }

  public jpvmBufferElementContents(char d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmChar;
    if (stride == 1) {
      if (inPlace)
        charArray = d;
      else {
        charArray = new char[n];
        System.arraycopy(d, 0, charArray, 0, n);
      }
    } else {
      charArray = new char[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        charArray[i] = d[j];
    }
  }

  public jpvmBufferElementContents(float d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmFloat;
    if (stride == 1) {
      if (inPlace)
        floatArray = d;
      else {
        floatArray = new float[n];
        System.arraycopy(d, 0, floatArray, 0, n);
      }
    } else {
      floatArray = new float[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        floatArray[i] = d[j];
    }
  }

  public jpvmBufferElementContents(double d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmDouble;
    if (stride == 1) {
      if (inPlace)
        doubleArray = d;
      else {
        doubleArray = new double[n];
        System.arraycopy(d, 0, doubleArray, 0, n);
      }
    } else {
      doubleArray = new double[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        doubleArray[i] = d[j];
    }
  }

  public jpvmBufferElementContents(byte d[], int n, int stride,
                                   boolean inPlace) {
    init();
    dataType = jpvmDataType.jpvmByte;
    if (stride == 1) {
      if (inPlace)
        byteArray = d;
      else {
        byteArray = new byte[n];
        System.arraycopy(d, 0, byteArray, 0, n);
      }
    } else {
      byteArray = new byte[n];
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        byteArray[i] = d[j];
    }
  }

  public void unpack(int d[], int n, int stride)
      throws jpvmException {
    if (dataType != jpvmDataType.jpvmInteger) {
      throw new jpvmException("buffer type mismatch, upkint.");
    }
    if (stride == 1) {
      System.arraycopy(intArray, 0, d, 0, n);
    } else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = intArray[i];
    }
  }

  public void unpack(short d[], int n, int stride) throws jpvmException {
    if (dataType != jpvmDataType.jpvmShort) {
      throw new jpvmException("buffer type mismatch, upkshort.");
    }
    if (stride == 1) {
      System.arraycopy(shortArray, 0, d, 0, n);
    } else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = shortArray[i];
    }
  }

  public void unpack(byte d[], int n, int stride) throws jpvmException {
    if (dataType != jpvmDataType.jpvmByte) {
      throw new jpvmException("buffer type mismatch, upkbyte.");
    }
    if (stride == 1)
      System.arraycopy(byteArray, 0, d, 0, n);
    else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = byteArray[i];
    }
  }

  public void unpack(char d[], int n, int stride) throws jpvmException {
    if (dataType != jpvmDataType.jpvmChar) {
      throw new jpvmException("buffer type mismatch, upkchar.");
    }
    if (stride == 1) {
      System.arraycopy(charArray, 0, d, 0, n);
    } else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = charArray[i];
    }
  }

  public void unpack(long d[], int n, int stride) throws jpvmException {
    if (dataType != jpvmDataType.jpvmLong) {
      throw new jpvmException("buffer type mismatch, upklong.");
    }
    if (stride == 1) {
      System.arraycopy(longArray, 0, d, 0, n);
    } else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = longArray[i];
    }
  }

  public void unpack(double d[], int n, int stride) throws jpvmException {
    if (dataType != jpvmDataType.jpvmDouble) {
      throw new jpvmException("buffer type mismatch, upkdouble.");
    }
    if (stride == 1) {
      System.arraycopy(doubleArray, 0, d, 0, n);
    } else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = doubleArray[i];
    }
  }

  public void unpack(float d[], int n, int stride) throws jpvmException {
    if (dataType != jpvmDataType.jpvmFloat) {
      throw new jpvmException("buffer type mismatch, upkfloat.");
    }
    if (stride == 1) {
      System.arraycopy(floatArray, 0, d, 0, n);
    } else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = floatArray[i];
    }
  }

  public void unpack(jpvmTaskId d[], int n, int stride)
      throws jpvmException {
    if (dataType != jpvmDataType.jpvmTid) {
      throw new jpvmException("buffer type mismatch, upktid.");
    }
    if (stride == 1) {
      System.arraycopy(taskArray, 0, d, 0, n);
    } else {
      int i, j;
      for (i = 0, j = 0; i < n; i++, j += stride)
        d[j] = taskArray[i];
    }
  }

  public String unpack() throws jpvmException {
    if (dataType != jpvmDataType.jpvmString) {
      throw new jpvmException("buffer type mismatch, upkstring.");
    }
    return new String(charArray);
  }
}

