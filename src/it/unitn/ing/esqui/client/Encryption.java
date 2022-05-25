package it.unitn.ing.esqui.client;

import org.logi.crypto.*;
import org.logi.crypto.keys.*;

import java.math.*;
import java.io.*;

/** EncryptionDES.java
 * <br>
 * Title:			<b>ESQUI Client EncryptionDES</b>
 * </br>
 * Description:	Encryption for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		August 2001
 * @comment:		none
 */

class Encryption {

  BlowfishKey newkey = null;

  public Encryption() {
//	Initialize the crypto algorithm and define the algorithm
    Crypto.initRandom();
    newkey = new BlowfishKey((Client.getSetting(2).getBytes()));
  }

  String encrypt(String password) {
//	Complete the password if length is different from 8 or 16 bytes
    StringBuffer passwordBuffer = new StringBuffer(password);
    int len = password.length();
    while (len < 16) {
      passwordBuffer = passwordBuffer.append(" ");
      len = passwordBuffer.length();
    }
//	Get the password in bytes
    byte[] passwordBytes = (passwordBuffer.toString()).getBytes();
    return finalPassword(passwordBytes, 1);
  }

  String decrypt(String password) {
//	Get the password in bytes
    byte[] passwordBytes = password.getBytes();
    String finalPassword = finalPassword(passwordBytes, 2);
    int mark;
    if ((mark = finalPassword.indexOf(" ")) != -1)
      finalPassword = finalPassword.substring(0, mark);
    return finalPassword;
  }

//Method for encrypting or decrypting (up to TYPE variable)
  String finalPassword(byte[] passwordBytes, int type) {
    ByteArrayOutputStream buffer = null;
    StringBuffer outTotal = null;
//	Check the password length
    boolean longPassword = false;
    if (passwordBytes.length == 16)
      longPassword = true;
//	Decrypt 8 bytes each time
    buffer = new ByteArrayOutputStream();
    outTotal = new StringBuffer();
    byte[] tmpOut = new byte[8];
    int i = 0;
//	Loop 2 times if long password (>8 bytes)
    do {
      buffer.write(passwordBytes, i, 8);
      switch (type) {
        case 1:
          newkey.encrypt(buffer.toByteArray(), 0, tmpOut, 0);
          break;
        case 2:
          newkey.decrypt(buffer.toByteArray(), 0, tmpOut, 0);
      }
      outTotal.append(new String(tmpOut));
      buffer.reset();
      if (i == 8)
        longPassword = false;
      i += 8;
    } while (longPassword);
    return outTotal.toString();
  }
}
