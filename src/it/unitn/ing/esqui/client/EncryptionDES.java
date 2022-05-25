package it.unitn.ing.esqui.client;

import java.security.*;
import javax.crypto.*;
import javax.crypto.spec.*;

/** EncryptionDES.java
 * <br>
 * Title:			<b>ESQUI Client EncryptionDES</b>
 * </br>
 * Description:	Encryption for the ESQUI client
 * @author:			Leonardo Cont, December 2000
 * @revision:		August 2001
 * @comment:		not used because it requires a certificate (or the JCE classes
 installed in the ...jre\lib\ext directory!)
 */

class EncryptionDES {

  Cipher cipher;
  SecretKeySpec skeySpec;

  public EncryptionDES() throws Exception {

    // Install temporary the SunJCE provider
    Provider sunJce = new com.sun.crypto.provider.SunJCE();
    Security.addProvider(sunJce);

    KeyGenerator kgen = KeyGenerator.getInstance("DES");
    SecretKey skey = kgen.generateKey();
    byte[] raw = skey.getEncoded();
    skeySpec = new SecretKeySpec(raw, "DES");

    cipher = Cipher.getInstance("DES/ECB/PKCS5Padding"); // or Cipher.getInstance("DES");
  }

  String encrypt(String password) throws Exception {
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec);
    byte[] encrypted = cipher.doFinal(password.getBytes());
    return new String(encrypted);
  }

  String decrypt(byte[] encryptedpassword) throws Exception {
    cipher.init(Cipher.DECRYPT_MODE, skeySpec);
    byte[] decrypted = cipher.doFinal(encryptedpassword);
    return new String(decrypted);
  }
}
