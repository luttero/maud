/*
 * @(#)MD5.java						0.3-3 06/05/2001
 *
 *  This file is part of the HTTPClient package
 *  Copyright (C) 1996-2001 Ronald Tschal\ufffdr
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *  MA 02111-1307, USA
 *
 *  For questions, suggestions, bug-reports, enhancement-requests etc.
 *  I may be contacted at:
 *
 *  ronald@innovation.ch
 *
 *  The HTTPClient's home page is located at:
 *
 *  http://www.innovation.ch/java/HTTPClient/ 
 *
 */

package HTTPClient;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;


/**
 * Some utility methods for digesting info using MD5.
 *
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 * @since	V0.3-3
 */
class MD5
{
    private static final char[] hex = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
    };

    /**
     * Turns array of bytes into string representing each byte as
     * unsigned hex number.
     *
     * @param hash	array of bytes to convert to hex-string
     * @return	generated hex string
     */
    public static final String toHex(byte hash[])
    {
	StringBuffer buf = new StringBuffer(hash.length * 2);

	for (int idx=0; idx<hash.length; idx++)
	    buf.append(hex[(hash[idx] >> 4) & 0x0f]).append(hex[hash[idx] & 0x0f]);

	return buf.toString();
    }

    /**
     * Digest the input.
     *
     * @param input the data to be digested.
     * @return the md5-digested input
     */
    public static final byte[] digest(byte[] input)
    {
	try 
	{
	    MessageDigest md5 = MessageDigest.getInstance("MD5");
	    return md5.digest(input);
	}
	catch (NoSuchAlgorithmException nsae)
	{
	    throw new Error(nsae.toString());
	}
    }

    /**
     * Digest the input.
     *
     * @param input1 the first part of the data to be digested.
     * @param input2 the second part of the data to be digested.
     * @return the md5-digested input
     */
    public static final byte[] digest(byte[] input1, byte[] input2)
    {
	try 
	{
	    MessageDigest md5 = MessageDigest.getInstance("MD5");
	    md5.update(input1);
	    return md5.digest(input2);
	}
	catch (NoSuchAlgorithmException nsae)
	{
	    throw new Error(nsae.toString());
	}
    }

    /**
     * Digest the input.
     *
     * @param input the data to be digested.
     * @return the md5-digested input as a hex string
     */
    public static final String hexDigest(byte[] input)
    {
	return toHex(digest(input));
    }

    /**
     * Digest the input.
     *
     * @param input1 the first part of the data to be digested.
     * @param input2 the second part of the data to be digested.
     * @return the md5-digested input as a hex string
     */
    public static final String hexDigest(byte[] input1, byte[] input2)
    {
	return toHex(digest(input1, input2));
    }

    /**
     * Digest the input.
     *
     * @param input the data to be digested.
     * @return the md5-digested input as a hex string
     */
    public static final byte[] digest(String input)
    {
	try
	    { return digest(input.getBytes("8859_1")); }
	catch (UnsupportedEncodingException uee)
	    { throw new Error(uee.toString()); }
    }

    /**
     * Digest the input.
     *
     * @param input the data to be digested.
     * @return the md5-digested input as a hex string
     */
    public static final String hexDigest(String input)
    {
	try
	    { return toHex(digest(input.getBytes("8859_1"))); }
	catch (UnsupportedEncodingException uee)
	    { throw new Error(uee.toString()); }
    }
}
