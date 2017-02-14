/*
 * @(#)Cookie.java					0.3-3 06/05/2001
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

import java.io.Serializable;
import java.net.ProtocolException;
import java.util.Date;


/**
 * This class represents an http cookie as specified in <a
 * href="http://home.netscape.com/newsref/std/cookie_spec.html">Netscape's
 * cookie spec</a>; however, because not even Netscape follows their own spec,
 * and because very few folks out there actually read specs but instead just
 * look whether Netscape accepts their stuff, the Set-Cookie header field
 * parser actually tries to follow what Netscape has implemented, instead of
 * what the spec says. Additionally, the parser it will also recognize the
 * Max-Age parameter from <a
 * href="http://www.ietf.org/rfc/rfc2109.txt">rfc-2109</a>, as that uses the
 * same header field (Set-Cookie).
 *
 * <P>Some notes about how Netscape (4.7) parses:
 * <ul>
 * <LI>Quoting: only quotes around the expires value are recognized as such;
 *     quotes around any other value are treated as part of the value.
 * <LI>White space: white space around names and values is ignored
 * <LI>Default path: if no path parameter is given, the path defaults to the
 *     path in the request-uri up to, but not including, the last '/'. Note
 *     that this is entirely different from what the spec says.
 * <LI>Commas and other delimiters: Netscape just parses until the next ';'.
 *     This means will allow commas etc inside values.
 * </ul>
 *
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 * @since	V0.3
 */
public class Cookie implements Serializable
{
    /** Make this compatible with V0.3-2 */
    private static final long serialVersionUID = 8599975325569296615L;

    protected String  name;
    protected String  value;
    protected Date    expires;
    protected String  domain;
    protected String  path;
    protected boolean secure;


    /**
     * Create a cookie.
     *
     * @param name    the cookie name
     * @param value   the cookie value
     * @param domain  the host this cookie will be sent to
     * @param path    the path prefix for which this cookie will be sent
     * @param epxires the Date this cookie expires, null if at end of
     *                session
     * @param secure  if true this cookie will only be over secure connections
     * @exception NullPointerException if <var>name</var>, <var>value</var>,
     *                                 <var>domain</var>, or <var>path</var>
     *                                 is null
     * @since V0.3-1
     */
    public Cookie(String name, String value, String domain, String path,
		  Date expires, boolean secure)
    {
	if (name == null)   throw new NullPointerException("missing name");
	if (value == null)  throw new NullPointerException("missing value");
	if (domain == null) throw new NullPointerException("missing domain");
	if (path == null)   throw new NullPointerException("missing path");

	this.name    = name;
	this.value   = value;
	this.domain  = domain.toLowerCase();
	this.path    = path;
	this.expires = expires;
	this.secure  = secure;

	if (this.domain.indexOf('.') == -1)  this.domain += ".local";
    }


    /**
     * Use <code>parse()</code> to create cookies.
     *
     * @see #parse(java.lang.String, HTTPClient.RoRequest)
     */
    protected Cookie(RoRequest req)
    {
	name    = null;
	value   = null;
	expires = null;
	domain  = req.getConnection().getHost();
	if (domain.indexOf('.') == -1)  domain += ".local";
	path    = Util.getPath(req.getRequestURI());
	/* This does not follow netscape's spec at all, but it's the way
	 * netscape seems to do it, and because people rely on that we
	 * therefore also have to do it...
	 */
	int slash = path.lastIndexOf('/');
	if (slash >= 0)
	    path = path.substring(0, slash);
	secure = false;
    }


    /**
     * Parses the Set-Cookie header into an array of Cookies.
     *
     * @param set_cookie the Set-Cookie header received from the server
     * @param req the request used
     * @return an array of Cookies as parsed from the Set-Cookie header
     * @exception ProtocolException if an error occurs during parsing
     */
    protected static Cookie[] parse(String set_cookie, RoRequest req)
		throws ProtocolException
    {
        int    beg = 0,
               end = 0,
	       start = 0;
        char[] buf = set_cookie.toCharArray();
        int    len = buf.length;

        Cookie cookie_arr[] = new Cookie[0], curr;


        cookies: while (true)                    // get all cookies
        {
            beg = Util.skipSpace(buf, beg);
            if (beg >= len)  break;	// no more left
	    if (buf[beg] == ',')	// empty header
	    {
		beg++;
		continue;
	    }

	    curr  = new Cookie(req);
	    start = beg;

	    // get cookie name and value first

	    end = set_cookie.indexOf('=', beg);
	    if (end == -1)
		throw new ProtocolException("Bad Set-Cookie header: " +
					    set_cookie + "\nNo '=' found " +
					    "for token starting at " +
					    "position " + beg);
	    curr.name = set_cookie.substring(beg, end).trim(); 

	    beg = Util.skipSpace(buf, end+1);
	    int comma = set_cookie.indexOf(',', beg);
	    int semic = set_cookie.indexOf(';', beg);
	    if (comma == -1  &&  semic == -1)  end = len;
	    else if (comma == -1)  end = semic;
	    else if (semic == -1)  end = comma;
	    else
	    {
		if (comma > semic)
		    end = semic;
		else
		{
		    // try to handle broken servers which put commas
		    // into cookie values
		    int eq = set_cookie.indexOf('=', comma);
		    if (eq > 0  &&  eq < semic)
			end = set_cookie.lastIndexOf(',', eq);
		    else
			end = semic;
		}
	    }
	    curr.value = set_cookie.substring(beg, end).trim();

	    beg = end;

	    // now parse attributes

	    boolean legal = true;
	    parts: while (true)			// parse all parts
	    {
		if (beg >= len  ||  buf[beg] == ',')  break;

		// skip empty fields
		if (buf[beg] == ';')
		{
		    beg = Util.skipSpace(buf, beg+1);
		    continue;
		}

		// first check for secure, as this is the only one w/o a '='
		if ((beg+6 <= len)  &&
		    set_cookie.regionMatches(true, beg, "secure", 0, 6))
		{
		    curr.secure = true;
		    beg += 6;

		    beg = Util.skipSpace(buf, beg);
		    if (beg < len  &&  buf[beg] == ';')	// consume ";"
			beg = Util.skipSpace(buf, beg+1);
		    else if (beg < len  &&  buf[beg] != ',')
			throw new ProtocolException("Bad Set-Cookie header: " +
						    set_cookie + "\nExpected " +
						    "';' or ',' at position " +
						    beg);

		    continue;
		}

		// alright, must now be of the form x=y
		end = set_cookie.indexOf('=', beg);
		if (end == -1)
		    throw new ProtocolException("Bad Set-Cookie header: " +
						set_cookie + "\nNo '=' found " +
						"for token starting at " +
						"position " + beg);

		String name = set_cookie.substring(beg, end).trim();
		beg = Util.skipSpace(buf, end+1);

		if (name.equalsIgnoreCase("expires"))
		{
		    /* Netscape ignores quotes around the date, and some twits
		     * actually send that...
		     */
		    if (set_cookie.charAt(beg) == '\"')
			beg = Util.skipSpace(buf, beg+1);

		    /* cut off the weekday if it is there. This is a little
		     * tricky because the comma is also used between cookies
		     * themselves. To make sure we don't inadvertantly
		     * mistake a date for a weekday we only skip letters.
		     */
		    int pos = beg;
		    while (pos < len  &&
			   (buf[pos] >= 'a'  &&  buf[pos] <= 'z'  ||
			    buf[pos] >= 'A'  &&  buf[pos] <= 'Z'))
			pos++;
		    pos = Util.skipSpace(buf, pos);
		    if (pos < len  &&  buf[pos] == ','  &&  pos > beg)
			beg = pos+1;
		}

		comma = set_cookie.indexOf(',', beg);
		semic = set_cookie.indexOf(';', beg);
		if (comma == -1  &&  semic == -1)  end = len;
		else if (comma == -1)  end = semic;
		else if (semic == -1)  end = comma;
		else end = Math.min(comma, semic);

		String value = set_cookie.substring(beg, end).trim();
		legal &= setAttribute(curr, name, value, set_cookie);

		beg = end;
		if (beg < len  &&  buf[beg] == ';')	// consume ";"
		    beg = Util.skipSpace(buf, beg+1);
	    }

	    if (legal)
	    {
		cookie_arr = Util.resizeArray(cookie_arr, cookie_arr.length+1);
		cookie_arr[cookie_arr.length-1] = curr;
	    } else
		Log.write(Log.COOKI, "Cooki: Ignoring cookie: " + curr);
	}

	return cookie_arr;
    }

    /**
     * Set the given attribute, if valid.
     *
     * @param cookie     the cookie on which to set the value
     * @param name       the name of the attribute
     * @param value      the value of the attribute
     * @param set_cookie the complete Set-Cookie header
     * @return true if the attribute is legal; false otherwise
     */
    private static boolean setAttribute(Cookie cookie, String name,
					String value, String set_cookie)
	    throws ProtocolException
    {
	if (name.equalsIgnoreCase("expires"))
	{
	    if (value.charAt(value.length()-1) == '\"')
		value = value.substring(0, value.length()-1).trim();
	    try
		// This is too strict...
		// { cookie.expires = Util.parseHttpDate(value); }
		{ cookie.expires = new Date(value); }
	    catch (IllegalArgumentException iae)
	    {
		/* More broken servers to deal with... Ignore expires
		 * if it's invalid
		throw new ProtocolException("Bad Set-Cookie header: " +
				    set_cookie + "\nInvalid date found at " +
				    "position " + beg);
		*/
		Log.write(Log.COOKI, "Cooki: Bad Set-Cookie header: " + set_cookie +
				     "\n       Invalid date `" + value + "'");
	    }
	}
	else if (name.equals("max-age"))	// from rfc-2109
	{
	    if (cookie.expires != null)  return true;
	    if (value.charAt(0) == '\"'  &&  value.charAt(value.length()-1) == '\"')
		value = value.substring(1, value.length()-1).trim();
	    int age;
	    try
		{ age = Integer.parseInt(value); }
	    catch (NumberFormatException nfe)
	    {
		throw new ProtocolException("Bad Set-Cookie header: " +
				    set_cookie + "\nMax-Age '" + value +
				    "' not a number");
	    }
	    cookie.expires = new Date(System.currentTimeMillis() + age*1000L);
	}
	else if (name.equalsIgnoreCase("domain"))
	{
	    // you get everything these days...
	    if (value.length() == 0)
	    {
		Log.write(Log.COOKI, "Cooki: Bad Set-Cookie header: " + set_cookie +
				     "\n       domain is empty - ignoring domain");
		return true;
	    }

	    // domains are case insensitive.
	    value = value.toLowerCase();

	    // add leading dot, if missing
	    if (value.length() != 0 && value.charAt(0) != '.'  &&
		!value.equals(cookie.domain))
		value = '.' + value;

	    // must be the same domain as in the url
	    if (!cookie.domain.endsWith(value))
	    {
		Log.write(Log.COOKI, "Cooki: Bad Set-Cookie header: " + set_cookie +
				     "\n       Current domain " + cookie.domain +
				     " does not match given parsed " + value);
		return false;
	    }


	    /* Netscape's original 2-/3-dot rule really doesn't work because
	     * many countries use a shallow hierarchy (similar to the special
	     * TLDs defined in the spec). While the rules in rfc-2965 aren't
	     * perfect either, they are better. OTOH, some sites use a domain
	     * so that the host name minus the domain name contains a dot (e.g.
	     * host x.x.yahoo.com and domain .yahoo.com). So, for the seven
	     * special TLDs we use the 2-dot rule, and for all others we use
	     * the rules in the state-man draft instead.
	     */

	    // domain must be either .local or must contain at least
	    // two dots
	    if (!value.equals(".local")  && value.indexOf('.', 1) == -1)
	    {
		Log.write(Log.COOKI, "Cooki: Bad Set-Cookie header: " + set_cookie +
				     "\n       Domain attribute " + value +
				     "isn't .local and doesn't have at " +
				     "least 2 dots");
		return false;
	    }

	    // If TLD not special then host minus domain may not
	    // contain any dots
	    String top = null;
	    if (value.length() > 3 )
		top = value.substring(value.length()-4);
	    if (top == null  ||  !(
		top.equalsIgnoreCase(".com")  ||
		top.equalsIgnoreCase(".edu")  ||
		top.equalsIgnoreCase(".net")  ||
		top.equalsIgnoreCase(".org")  ||
		top.equalsIgnoreCase(".gov")  ||
		top.equalsIgnoreCase(".mil")  ||
		top.equalsIgnoreCase(".int")))
	    {
		int dl = cookie.domain.length(), vl = value.length();
		if (dl > vl  &&
		    cookie.domain.substring(0, dl-vl).indexOf('.') != -1)
		{
		    Log.write(Log.COOKI, "Cooki: Bad Set-Cookie header: " + set_cookie +
					 "\n       Domain attribute " + value +
					 "is more than one level below " +
					 "current domain " + cookie.domain);
		    return false;
		}
	    }

	    cookie.domain = value;
	}
	else if (name.equalsIgnoreCase("path"))
	    cookie.path = value;
	else
	  ; // unknown attribute - ignore

	return true;
    }


    /**
     * Return the name of this cookie.
     */
    public String getName()
    {
	return name;
    }


    /**
     * Return the value of this cookie.
     */
    public String getValue()
    {
	return value;
    }


    /**
     * @return the expiry date of this cookie, or null if none set.
     */
    public Date expires()
    {
	return expires;
    }


    /**
     * @return true if the cookie should be discarded at the end of the
     *         session; false otherwise
     */
    public boolean discard()
    {
	return (expires == null);
    }


    /**
     * Return the domain this cookie is valid in.
     */
    public String getDomain()
    {
	return domain;
    }


    /**
     * Return the path this cookie is associated with.
     */
    public String getPath()
    {
	return path;
    }


    /**
     * Return whether this cookie should only be sent over secure connections.
     */
    public boolean isSecure()
    {
	return secure;
    }


    /**
     * @return true if this cookie has expired
     */
    public boolean hasExpired()
    {
	return (expires != null  &&  expires.getTime() <= System.currentTimeMillis());
    }


    /**
     * @param  req  the request to be sent
     * @return true if this cookie should be sent with the request
     */
    protected boolean sendWith(RoRequest req)
    {
	HTTPConnection con = req.getConnection();
	String eff_host = con.getHost();
	if (eff_host.indexOf('.') == -1)  eff_host += ".local";

	return ((domain.charAt(0) == '.'  &&  eff_host.endsWith(domain)  ||
		 domain.charAt(0) != '.'  &&  eff_host.equals(domain))  &&
		Util.getPath(req.getRequestURI()).startsWith(path)  &&
		(!secure || con.getProtocol().equals("https") ||
		 con.getProtocol().equals("shttp")));
    }


    /**
     * Hash up name, path and domain into new hash.
     */
    public int hashCode()
    {
	return (name.hashCode() + path.hashCode() + domain.hashCode());
    }


    /**
     * Two cookies match if the name, path and domain match.
     */
    public boolean equals(Object obj)
    {
	if ((obj != null) && (obj instanceof Cookie))
	{
	    Cookie other = (Cookie) obj;
	    return  (this.name.equals(other.name)  &&
		     this.path.equals(other.path)  &&
		     this.domain.equals(other.domain));
	}
	return false;
    }


    /**
     * @return a string suitable for sending in a Cookie header.
     */
    protected String toExternalForm()
    {
	return name + "=" + value;
    }


    /**
     * Create a string containing all the cookie fields. The format is that
     * used in the Set-Cookie header.
     */
    public String toString()
    {
	StringBuffer res = new StringBuffer(name.length() + value.length() + 30);
	res.append(name).append('=').append(value);
	if (expires != null)  res.append("; expires=").append(expires);
	if (path != null)     res.append("; path=").append(path);
	if (domain != null)   res.append("; domain=").append(domain);
	if (secure)           res.append("; secure");
	return res.toString();
    }
}
