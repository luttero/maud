/*
 * @(#)URI.java						0.3-3 06/05/2001
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

import java.net.URL;
import java.net.MalformedURLException;
import java.util.BitSet;
import java.util.Hashtable;

/**
 * This class represents a generic URI, as defined in RFC-2396.
 * This is similar to java.net.URL, with the following enhancements:
 * <UL>
 * <LI>it doesn't require a URLStreamhandler to exist for the scheme; this
 *     allows this class to be used to hold any URI, construct absolute
 *     URIs from relative ones, etc.
 * <LI>it handles escapes correctly
 * <LI>equals() works correctly
 * <LI>relative URIs are correctly constructed
 * <LI>it has methods for accessing various fields such as userinfo,
 *     fragment, params, etc.
 * <LI>it handles less common forms of resources such as the "*" used in
 *     http URLs.
 * </UL>
 *
 * <P>The elements are always stored in escaped form.
 *
 * <P>While RFC-2396 distinguishes between just two forms of URI's, those that
 * follow the generic syntax and those that don't, this class knows about a
 * third form, named semi-generic, used by quite a few popular schemes.
 * Semi-generic syntax treats the path part as opaque, i.e. has the form
 * &lt;scheme&gt;://&lt;authority&gt;/&lt;opaque&gt; . Relative URI's of this
 * type are only resolved as far as absolute paths - relative paths do not
 * exist.
 *
 * <P>Ideally, java.net.URL should subclass URI.
 *
 * @see		<A HREF="http://www.ics.uci.edu/pub/ietf/uri/rfc2396.txt">rfc-2396</A>
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 * @since	V0.3-1
 */
public class URI
{
    /**
     * If true, then the parser will resolve certain URI's in backwards
     * compatible (but technically incorrect) manner. Example:
     *
     *<PRE>
     * base   = http://a/b/c/d;p?q
     * rel    = http:g
     * result = http:g		(correct)
     * result = http://a/b/c/g	(backwards compatible)
     *</PRE>
     *
     * See rfc-2396, section 5.2, step 3, second paragraph.
     */
    public static final boolean ENABLE_BACKWARDS_COMPATIBILITY = true;

    protected static final Hashtable defaultPorts          = new Hashtable();
    protected static final Hashtable usesGenericSyntax     = new Hashtable();
    protected static final Hashtable usesSemiGenericSyntax = new Hashtable();

    /* various character classes as defined in the draft */
    protected static final BitSet alphanumChar;
    protected static final BitSet markChar;
    protected static final BitSet reservedChar;
    protected static final BitSet unreservedChar;
    protected static final BitSet uricChar;
    protected static final BitSet pcharChar;
    protected static final BitSet userinfoChar;
    protected static final BitSet schemeChar;
    protected static final BitSet hostChar;
    protected static final BitSet opaqueChar;
    protected static final BitSet reg_nameChar;

    /* These are not directly in the spec, but used for escaping and
     * unescaping parts
     */

    /** list of characters which must not be unescaped when unescaping a scheme */
    public static final BitSet resvdSchemeChar;
    /** list of characters which must not be unescaped when unescaping a userinfo */
    public static final BitSet resvdUIChar;
    /** list of characters which must not be unescaped when unescaping a host */
    public static final BitSet resvdHostChar;
    /** list of characters which must not be unescaped when unescaping a path */
    public static final BitSet resvdPathChar;
    /** list of characters which must not be unescaped when unescaping a query string */
    public static final BitSet resvdQueryChar;
    /** list of characters which must not be escaped when escaping a path */
    public static final BitSet escpdPathChar;
    /** list of characters which must not be escaped when escaping a query string */
    public static final BitSet escpdQueryChar;
    /** list of characters which must not be escaped when escaping a fragment identifier */
    public static final BitSet escpdFragChar;

    static
    {
	defaultPorts.put("http",      new Integer(80));
	defaultPorts.put("shttp",     new Integer(80));
	defaultPorts.put("http-ng",   new Integer(80));
	defaultPorts.put("coffee",    new Integer(80));
	defaultPorts.put("https",     new Integer(443));
	defaultPorts.put("ftp",       new Integer(21));
	defaultPorts.put("telnet",    new Integer(23));
	defaultPorts.put("nntp",      new Integer(119));
	defaultPorts.put("news",      new Integer(119));
	defaultPorts.put("snews",     new Integer(563));
	defaultPorts.put("hnews",     new Integer(80));
	defaultPorts.put("smtp",      new Integer(25));
	defaultPorts.put("gopher",    new Integer(70));
	defaultPorts.put("wais",      new Integer(210));
	defaultPorts.put("whois",     new Integer(43));
	defaultPorts.put("whois++",   new Integer(63));
	defaultPorts.put("rwhois",    new Integer(4321));
	defaultPorts.put("imap",      new Integer(143));
	defaultPorts.put("pop",       new Integer(110));
	defaultPorts.put("prospero",  new Integer(1525));
	defaultPorts.put("irc",       new Integer(194));
	defaultPorts.put("ldap",      new Integer(389));
	defaultPorts.put("nfs",       new Integer(2049));
	defaultPorts.put("z39.50r",   new Integer(210));
	defaultPorts.put("z39.50s",   new Integer(210));
	defaultPorts.put("vemmi",     new Integer(575));
	defaultPorts.put("videotex",  new Integer(516));
	defaultPorts.put("cmp",       new Integer(829));

	usesGenericSyntax.put("http", Boolean.TRUE);
	usesGenericSyntax.put("https", Boolean.TRUE);
	usesGenericSyntax.put("shttp", Boolean.TRUE);
	usesGenericSyntax.put("coffee", Boolean.TRUE);
	usesGenericSyntax.put("ftp", Boolean.TRUE);
	usesGenericSyntax.put("file", Boolean.TRUE);
	usesGenericSyntax.put("nntp", Boolean.TRUE);
	usesGenericSyntax.put("news", Boolean.TRUE);
	usesGenericSyntax.put("snews", Boolean.TRUE);
	usesGenericSyntax.put("hnews", Boolean.TRUE);
	usesGenericSyntax.put("imap", Boolean.TRUE);
	usesGenericSyntax.put("wais", Boolean.TRUE);
	usesGenericSyntax.put("nfs", Boolean.TRUE);
	usesGenericSyntax.put("sip", Boolean.TRUE);
	usesGenericSyntax.put("sips", Boolean.TRUE);
	usesGenericSyntax.put("sipt", Boolean.TRUE);
	usesGenericSyntax.put("sipu", Boolean.TRUE);
	/* Note: schemes which definitely don't use the generic-URI syntax
	 * and must therefore never appear in the above list:
	 * "urn", "mailto", "sdp", "service", "tv", "gsm-sms", "tel", "fax",
	 * "modem", "eid", "cid", "mid", "data", "ldap"
	 */

	usesSemiGenericSyntax.put("ldap", Boolean.TRUE);
	usesSemiGenericSyntax.put("irc", Boolean.TRUE);
	usesSemiGenericSyntax.put("gopher", Boolean.TRUE);
	usesSemiGenericSyntax.put("videotex", Boolean.TRUE);
	usesSemiGenericSyntax.put("rwhois", Boolean.TRUE);
	usesSemiGenericSyntax.put("whois++", Boolean.TRUE);
	usesSemiGenericSyntax.put("smtp", Boolean.TRUE);
	usesSemiGenericSyntax.put("telnet", Boolean.TRUE);
	usesSemiGenericSyntax.put("prospero", Boolean.TRUE);
	usesSemiGenericSyntax.put("pop", Boolean.TRUE);
	usesSemiGenericSyntax.put("vemmi", Boolean.TRUE);
	usesSemiGenericSyntax.put("z39.50r", Boolean.TRUE);
	usesSemiGenericSyntax.put("z39.50s", Boolean.TRUE);
	usesSemiGenericSyntax.put("stream", Boolean.TRUE);
	usesSemiGenericSyntax.put("cmp", Boolean.TRUE);

	alphanumChar = new BitSet(128);
	for (int ch='0'; ch<='9'; ch++)  alphanumChar.set(ch);
	for (int ch='A'; ch<='Z'; ch++)  alphanumChar.set(ch);
	for (int ch='a'; ch<='z'; ch++)  alphanumChar.set(ch);

	markChar = new BitSet(128);
	markChar.set('-');
	markChar.set('_');
	markChar.set('.');
	markChar.set('!');
	markChar.set('~');
	markChar.set('*');
	markChar.set('\'');
	markChar.set('(');
	markChar.set(')');

	reservedChar = new BitSet(128);
	reservedChar.set(';');
	reservedChar.set('/');
	reservedChar.set('?');
	reservedChar.set(':');
	reservedChar.set('@');
	reservedChar.set('&');
	reservedChar.set('=');
	reservedChar.set('+');
	reservedChar.set('$');
	reservedChar.set(',');

	unreservedChar = new BitSet(128);
	unreservedChar.or(alphanumChar);
	unreservedChar.or(markChar);

	uricChar = new BitSet(128);
	uricChar.or(unreservedChar);
	uricChar.or(reservedChar);
	uricChar.set('%');

	pcharChar = new BitSet(128);
	pcharChar.or(unreservedChar);
	pcharChar.set('%');
	pcharChar.set(':');
	pcharChar.set('@');
	pcharChar.set('&');
	pcharChar.set('=');
	pcharChar.set('+');
	pcharChar.set('$');
	pcharChar.set(',');

	userinfoChar = new BitSet(128);
	userinfoChar.or(unreservedChar);
	userinfoChar.set('%');
	userinfoChar.set(';');
	userinfoChar.set(':');
	userinfoChar.set('&');
	userinfoChar.set('=');
	userinfoChar.set('+');
	userinfoChar.set('$');
	userinfoChar.set(',');

	// this actually shouldn't contain uppercase letters...
	schemeChar = new BitSet(128);
	schemeChar.or(alphanumChar);
	schemeChar.set('+');
	schemeChar.set('-');
	schemeChar.set('.');

	opaqueChar = new BitSet(128);
	opaqueChar.or(uricChar);

	hostChar = new BitSet(128);
	hostChar.or(alphanumChar);
	hostChar.set('-');
	hostChar.set('.');

	reg_nameChar = new BitSet(128);
	reg_nameChar.or(unreservedChar);
	reg_nameChar.set('$');
	reg_nameChar.set(',');
	reg_nameChar.set(';');
	reg_nameChar.set(':');
	reg_nameChar.set('@');
	reg_nameChar.set('&');
	reg_nameChar.set('=');
	reg_nameChar.set('+');

	resvdSchemeChar = new BitSet(128);
	resvdSchemeChar.set(':');

	resvdUIChar = new BitSet(128);
	resvdUIChar.set('@');

	resvdHostChar = new BitSet(128);
	resvdHostChar.set(':');
	resvdHostChar.set('/');
	resvdHostChar.set('?');
	resvdHostChar.set('#');

	resvdPathChar = new BitSet(128);
	resvdPathChar.set('/');
	resvdPathChar.set(';');
	resvdPathChar.set('?');
	resvdPathChar.set('#');

	resvdQueryChar = new BitSet(128);
	resvdQueryChar.set('#');

	escpdPathChar = new BitSet(128);
	escpdPathChar.or(pcharChar);
	escpdPathChar.set('%');
	escpdPathChar.set('/');
	escpdPathChar.set(';');

	escpdQueryChar = new BitSet(128);
	escpdQueryChar.or(uricChar);
	escpdQueryChar.clear('#');

	escpdFragChar = new BitSet(128);
	escpdFragChar.or(uricChar);
    }


    /* our uri in pieces */

    protected static final int OPAQUE       = 0;
    protected static final int SEMI_GENERIC = 1;
    protected static final int GENERIC      = 2;

    protected int     type;
    protected String  scheme;
    protected String  opaque;
    protected String  userinfo;
    protected String  host;
    protected int     port = -1;
    protected String  path;
    protected String  query;
    protected String  fragment;


    /* cache the java.net.URL */

    protected URL     url = null;


    // Constructors

    /**
     * Constructs a URI from the given string representation. The string
     * must be an absolute URI.
     *
     * @param uri a String containing an absolute URI
     * @exception ParseException if no scheme can be found or a specified
     *                           port cannot be parsed as a number
     */
    public URI(String uri)  throws ParseException
    {
	this((URI) null, uri);
    }


    /**
     * Constructs a URI from the given string representation, relative to
     * the given base URI.
     *
     * @param base    the base URI, relative to which <var>rel_uri</var>
     *                is to be parsed
     * @param rel_uri a String containing a relative or absolute URI
     * @exception ParseException if <var>base</var> is null and
     *                           <var>rel_uri</var> is not an absolute URI, or
     *                           if <var>base</var> is not null and the scheme
     *                           is not known to use the generic syntax, or
     *                           if a given port cannot be parsed as a number
     */
    public URI(URI base, String rel_uri)  throws ParseException
    {
	/* Parsing is done according to the following RE:
	 *
	 *  ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
	 *   12            3  4          5       6  7        8 9
	 *
	 * 2: scheme
	 * 4: authority
	 * 5: path
	 * 7: query
	 * 9: fragment
	 */

	char[] uri = rel_uri.toCharArray();
	int pos = 0, idx, len = uri.length;


	// trim()

	while (pos < len  &&  Character.isWhitespace(uri[pos]))    pos++;
	while (len > 0    &&  Character.isWhitespace(uri[len-1]))  len--;


	// strip the special "url" or "uri" scheme

	if (pos < len-3  &&  uri[pos+3] == ':'  &&
	    (uri[pos+0] == 'u'  ||  uri[pos+0] == 'U')  &&
	    (uri[pos+1] == 'r'  ||  uri[pos+1] == 'R')  &&
	    (uri[pos+2] == 'i'  ||  uri[pos+2] == 'I'  ||
	     uri[pos+2] == 'l'  ||  uri[pos+2] == 'L'))
	    pos += 4;


	// get scheme: (([^:/?#]+):)?

	idx = pos;
	while (idx < len  &&  uri[idx] != ':'  &&  uri[idx] != '/'  &&
	       uri[idx] != '?'  &&  uri[idx] != '#')
	    idx++;
	if (idx < len  &&  uri[idx] == ':')
	{
	    scheme = rel_uri.substring(pos, idx).trim().toLowerCase();
	    pos = idx + 1;
	}


	// check and resolve scheme

	String final_scheme = scheme;
	if (scheme == null)
	{
	    if (base == null)
		throw new ParseException("No scheme found");
	    final_scheme = base.scheme;
	}


	// check for generic vs. opaque

	type = usesGenericSyntax(final_scheme) ? GENERIC :
	       usesSemiGenericSyntax(final_scheme) ? SEMI_GENERIC : OPAQUE;
	if (type == OPAQUE)
	{
	    if (base != null  &&  scheme == null)
		throw new ParseException("Can't resolve relative URI for " +
					 "scheme " + final_scheme);

	    opaque = escape(rel_uri.substring(pos), opaqueChar, true);
	    if (opaque.length() > 0  &&  opaque.charAt(0) == '/')
		opaque = "%2F" + opaque.substring(1);
	    return;
	}


	// get authority: (//([^/?#]*))?

	if (pos+1 < len  &&  uri[pos] == '/'  &&  uri[pos+1] == '/')
	{
	    pos += 2;
	    idx = pos;
	    while (idx < len  &&  uri[idx] != '/'  &&  uri[idx] != '?'  &&
		   uri[idx] != '#')
		idx++;

	    parse_authority(rel_uri.substring(pos, idx), final_scheme);
	    pos = idx;
	}


	// handle semi-generic and generic uri's
	
	if (type == SEMI_GENERIC)
	{
	    path = escape(rel_uri.substring(pos), uricChar, true);
	    if (path.length() > 0  &&  path.charAt(0) != '/')
		path = '/' + path;
	}
	else
	{
	    // get path: ([^?#]*)

	    idx = pos;
	    while (idx < len  &&  uri[idx] != '?'  &&  uri[idx] != '#')
		idx++;
	    path = escape(rel_uri.substring(pos, idx), escpdPathChar, true);
	    pos = idx;


	    // get query: (\?([^#]*))?

	    if (pos < len  &&  uri[pos] == '?')
	    {
		pos += 1;
		idx = pos;
		while (idx < len  &&  uri[idx] != '#')
		    idx++;
		this.query = escape(rel_uri.substring(pos, idx), escpdQueryChar, true);
		pos = idx;
	    }


	    // get fragment: (#(.*))?

	    if (pos < len  &&  uri[pos] == '#')
		this.fragment = escape(rel_uri.substring(pos+1, len), escpdFragChar, true);
	}


	// now resolve the parts relative to the base

	if (base != null)
	{
	    if (scheme != null  &&			// resolve scheme
		!(scheme.equals(base.scheme)  &&  ENABLE_BACKWARDS_COMPATIBILITY))
	      return;
	    scheme = base.scheme;

	    if (host != null)				// resolve authority
		return;
	    userinfo = base.userinfo;
	    host     = base.host;
	    port     = base.port;

	    if (type == SEMI_GENERIC)			// can't resolve relative paths
		return;

	    if (path.length() == 0  &&  query == null)	// current doc
	    {
		path  = base.path;
		query = base.query;
		return;
	    }

	    if (path.length() == 0  ||  path.charAt(0) != '/')	// relative path
	    {
		idx = (base.path != null) ? base.path.lastIndexOf('/') : -1;
		if (idx < 0)
		    path = '/' + path;
		else
		    path = base.path.substring(0, idx+1) + path;

		path = canonicalizePath(path);
	    }
	}
    }

    /**
     * Remove all "/../" and "/./" from path, where possible. Leading "/../"'s
     * are not removed.
     *
     * @param path the path to canonicalize
     * @return the canonicalized path
     */
    public static String canonicalizePath(String path)
    {
	int idx, len = path.length();
	if (!((idx = path.indexOf("/.")) != -1  &&
	      (idx == len-2  ||  path.charAt(idx+2) == '/'  ||
	       (path.charAt(idx+2) == '.'  &&
		(idx == len-3  ||  path.charAt(idx+3) == '/')) )))
	    return path;

	char[] p = new char[path.length()];		// clean path
	path.getChars(0, p.length, p, 0);

	int beg = 0;
	for (idx=1; idx<len; idx++)
	{
	    if (p[idx] == '.'  &&  p[idx-1] == '/')
	    {
		int end;
		if (idx == len-1)		// trailing "/."
		{
		    end  = idx;
		    idx += 1;
		}
		else if (p[idx+1] == '/')	// "/./"
		{
		    end  = idx - 1;
		    idx += 1;
		}
		else if (p[idx+1] == '.'  &&
			 (idx == len-2  ||  p[idx+2] == '/')) // "/../"
		{
		    if (idx < beg + 2)	// keep from backing up too much
		    {
			beg = idx + 2;
			continue;
		    }

		    end  = idx - 2;
		    while (end > beg  &&  p[end] != '/')  end--;
		    if (p[end] != '/')  continue;
		    if (idx == len-2) end++;
		    idx += 2;
		}
		else
		    continue;
		System.arraycopy(p, idx, p, end, len-idx);
		len -= idx - end;
		idx = end;
	    }
	}

	return new String(p, 0, len);
    }

    /**
     * Parse the authority specific part
     */
    private void parse_authority(String authority, String scheme)
	    throws ParseException
    {
	/* The authority is further parsed according to:
	 *
	 *  ^(([^@]*)@?)(\[[^]]*\]|[^:]*)?(:(.*))?
	 *   12         3       4 5
	 *
	 * 2: userinfo
	 * 3: host
	 * 5: port
	 */

	char[] uri = authority.toCharArray();
	int pos = 0, idx, len = uri.length;


	// get userinfo: (([^@]*)@?)

	idx = pos;
	while (idx < len  &&  uri[idx] != '@')
	    idx++;
	if (idx < len  &&  uri[idx] == '@')
	{
	    this.userinfo = escape(authority.substring(pos, idx), userinfoChar, true);
	    pos = idx + 1;
	}


	// get host: (\[[^]]*\]|[^:]*)?

	idx = pos;
	if (idx < len  &&  uri[idx] == '[')	// IPv6
	{
	    while (idx < len  &&  uri[idx] != ']')
		idx++;
	    if (idx == len)
		throw new ParseException("No closing ']' found for opening '['"+
					 " at position " + pos +
					 " in authority `" + authority + "'");
	    this.host = authority.substring(pos+1, idx);
	    idx++;
	}
	else
	{
	    while (idx < len  &&  uri[idx] != ':')
		idx++;
	    this.host = escape(authority.substring(pos, idx), uricChar, true);
	}
	pos = idx;


	// get port: (:(.*))?

	if (pos < (len-1)  &&  uri[pos] == ':')
	{
	    int p;
	    try
	    {
		p = Integer.parseInt(
			    unescape(authority.substring(pos+1, len), null));
		if (p < 0)  throw new NumberFormatException();
	    }
	    catch (NumberFormatException e)
	    {
		throw new ParseException(authority.substring(pos+1, len) +
					 " is an invalid port number");
	    }
	    if (p == defaultPort(scheme))
		this.port = -1;
	    else
		this.port = p;
	}
    }


    /**
     * Construct a URI from the given URL.
     *
     * @param url the URL
     * @exception ParseException if <code>url.toExternalForm()</code> generates
     *                           an invalid string representation
     */
    public URI(URL url)  throws ParseException
    {
	this((URI) null, url.toExternalForm());
    }


    /**
     * Constructs a URI from the given parts, using the default port for
     * this scheme (if known). The parts must be in unescaped form.
     *
     * @param scheme the scheme (sometimes known as protocol)
     * @param host   the host
     * @param path   the path part
     * @exception ParseException if <var>scheme</var> is null
     */
    public URI(String scheme, String host, String path)  throws ParseException
    {
	this(scheme, null, host, -1, path, null, null);
    }


    /**
     * Constructs a URI from the given parts. The parts must be in unescaped
     * form.
     *
     * @param scheme the scheme (sometimes known as protocol)
     * @param host   the host
     * @param port   the port
     * @param path   the path part
     * @exception ParseException if <var>scheme</var> is null
     */
    public URI(String scheme, String host, int port, String path)
	    throws ParseException
    {
	this(scheme, null, host, port, path, null, null);
    }


    /**
     * Constructs a URI from the given parts. Any part except for the
     * the scheme may be null. The parts must be in unescaped form.
     *
     * @param scheme   the scheme (sometimes known as protocol)
     * @param userinfo the userinfo
     * @param host     the host
     * @param port     the port
     * @param path     the path part
     * @param query    the query string
     * @param fragment the fragment identifier
     * @exception ParseException if <var>scheme</var> is null
     */
    public URI(String scheme, String userinfo, String host, int port,
	       String path, String query, String fragment)
	    throws ParseException
    {
	if (scheme == null)
	    throw new ParseException("missing scheme");
	this.scheme = escape(scheme.trim().toLowerCase(), schemeChar, true);
	if (userinfo != null)
	    this.userinfo = escape(userinfo.trim(), userinfoChar, true);
	if (host != null)
	{
	    host = host.trim();
	    this.host = isIPV6Addr(host) ? host : escape(host, hostChar, true);
	}
	if (port != defaultPort(scheme))
	    this.port     = port;
	if (path != null)
	    this.path     = escape(path.trim(), escpdPathChar, true);	// ???
	if (query != null)
	    this.query    = escape(query.trim(), escpdQueryChar, true);
	if (fragment != null)
	    this.fragment = escape(fragment.trim(), escpdFragChar, true);

	type = usesGenericSyntax(scheme) ? GENERIC : SEMI_GENERIC;
    }

    private static final boolean isIPV6Addr(String host)
    {
	if (host.indexOf(':') < 0)
	    return false;

	for (int idx=0; idx<host.length(); idx++)
	{
	    char ch = host.charAt(idx);
	    if ((ch < '0'  ||  ch > '9')  &&  ch != ':')
		return false;
	}

	return true;
    }


    /**
     * Constructs an opaque URI from the given parts.
     *
     * @param scheme the scheme (sometimes known as protocol)
     * @param opaque the opaque part
     * @exception ParseException if <var>scheme</var> is null
     */
    public URI(String scheme, String opaque)
	    throws ParseException
    {
	if (scheme == null)
	    throw new ParseException("missing scheme");
	this.scheme = escape(scheme.trim().toLowerCase(), schemeChar, true);
	this.opaque = escape(opaque, opaqueChar, true);

	type = OPAQUE;
    }


    // Class Methods

    /**
     * @return true if the scheme should be parsed according to the
     *         generic-URI syntax
     */
    public static boolean usesGenericSyntax(String scheme)
    {
	return usesGenericSyntax.containsKey(scheme.trim().toLowerCase());
    }


    /**
     * @return true if the scheme should be parsed according to a
     *         semi-generic-URI syntax &lt;scheme&tgt;://&lt;hostport&gt;/&lt;opaque&gt;
     */
    public static boolean usesSemiGenericSyntax(String scheme)
    {
	return usesSemiGenericSyntax.containsKey(scheme.trim().toLowerCase());
    }


    /**
     * Return the default port used by a given protocol.
     *
     * @param protocol the protocol
     * @return the port number, or 0 if unknown
     */
    public final static int defaultPort(String protocol)
    {
	Integer port = (Integer) defaultPorts.get(protocol.trim().toLowerCase());
	return (port != null) ? port.intValue() : 0;
    }


    // Instance Methods

    /**
     * @return the scheme (often also referred to as protocol)
     */
    public String getScheme()
    {
	return scheme;
    }


    /**
     * @return the opaque part, or null if this URI is generic
     */
    public String getOpaque()
    {
	return opaque;
    }


    /**
     * @return the host
     */
    public String getHost()
    {
	return host;
    }


    /**
     * @return the port, or -1 if it's the default port, or 0 if unknown
     */
    public int getPort()
    {
	return port;
    }


    /**
     * @return the user info
     */
    public String getUserinfo()
    {
	return userinfo;
    }


    /**
     * @return the path
     */
    public String getPath()
    {
	return path;
    }


    /**
     * @return the query string
     */
    public String getQueryString()
    {
	return query;
    }


    /**
     * @return the path and query
     */
    public String getPathAndQuery()
    {
	if (query == null)
	    return path;
	if (path == null)
	    return "?" + query;
	return path + "?" + query;
    }


    /**
     * @return the fragment
     */
    public String getFragment()
    {
	return fragment;
    }


    /**
     * Does the scheme specific part of this URI use the generic-URI syntax?
     *
     * <P>In general URI are split into two categories: opaque-URI and
     * generic-URI. The generic-URI syntax is the syntax most are familiar
     * with from URLs such as ftp- and http-URLs, which is roughly:
     * <PRE>
     * generic-URI = scheme ":" [ "//" server ] [ "/" ] [ path_segments ] [ "?" query ]
     * </PRE>
     * (see RFC-2396 for exact syntax). Only URLs using the generic-URI syntax
     * can be used to create and resolve relative URIs.
     *
     * <P>Whether a given scheme is parsed according to the generic-URI
     * syntax or wether it is treated as opaque is determined by an internal
     * table of URI schemes.
     *
     * @see <A HREF="http://www.ics.uci.edu/pub/ietf/uri/rfc2396.txt">rfc-2396</A>
     */
    public boolean isGenericURI()
    {
	return (type == GENERIC);
    }

    /**
     * Does the scheme specific part of this URI use the semi-generic-URI syntax?
     *
     * <P>Many schemes which don't follow the full generic syntax actually
     * follow a reduced form where the path part is treated is opaque. This
     * is used for example by ldap, smtp, pop, etc, and is roughly
     * <PRE>
     * generic-URI = scheme ":" [ "//" server ] [ "/" [ opaque_path ] ]
     * </PRE>
     * I.e. parsing is identical to the generic-syntax, except that the path
     * part is not further parsed. URLs using the semi-generic-URI syntax can
     * be used to create and resolve relative URIs with the restriction that
     * all paths are treated as absolute.
     *
     * <P>Whether a given scheme is parsed according to the semi-generic-URI
     * syntax is determined by an internal table of URI schemes.
     *
     * @see #isGenericURI()
     */
    public boolean isSemiGenericURI()
    {
	return (type == SEMI_GENERIC);
    }


    /**
     * Will try to create a java.net.URL object from this URI.
     *
     * @return the URL
     * @exception MalformedURLException if no handler is available for the
     *            scheme
     */
    public URL toURL()  throws MalformedURLException
    {
	if (url != null)  return url;

	if (opaque != null)
	    return (url = new URL(scheme + ":" + opaque));

	String hostinfo;
	if (userinfo != null  &&  host != null)
	    hostinfo = userinfo + "@" + host;
	else if (userinfo != null)
	    hostinfo = userinfo + "@";
	else
	    hostinfo = host;

	StringBuffer file = new StringBuffer(100);
	assemblePath(file, true, true, false);

	url = new URL(scheme, hostinfo, port, file.toString());
	return url;
    }


    private final void assemblePath(StringBuffer buf, boolean printEmpty,
				    boolean incFragment, boolean unescape)
    {
	if ((path == null  ||  path.length() == 0)  &&  printEmpty)
	    buf.append('/');

	if (path != null)
	    buf.append(unescape ? unescapeNoPE(path, resvdPathChar) : path);

	if (query != null)
	{
	    buf.append('?');
	    buf.append(unescape ? unescapeNoPE(query, resvdQueryChar) : query);
	}

	if (fragment != null  &&  incFragment)
	{
	    buf.append('#');
	    buf.append(unescape ? unescapeNoPE(fragment, null) : fragment);
	}
    }


    private final String stringify(boolean unescape)
    {
	StringBuffer uri = new StringBuffer(100);

	if (scheme != null)
	{
	    uri.append(unescape ? unescapeNoPE(scheme, resvdSchemeChar) : scheme);
	    uri.append(':');
	}

	if (opaque != null)		// it's an opaque-uri
	{
	    uri.append(unescape ? unescapeNoPE(opaque, null) : opaque);
	    return uri.toString();
	}

	if (userinfo != null  ||  host != null  ||  port != -1)
	    uri.append("//");

	if (userinfo != null)
	{
	    uri.append(unescape ? unescapeNoPE(userinfo, resvdUIChar) : userinfo);
	    uri.append('@');
	}

	if (host != null)
	{
	    if (host.indexOf(':') < 0)
		uri.append(unescape ? unescapeNoPE(host, resvdHostChar) : host);
	    else
		uri.append('[').append(host).append(']');
	}

	if (port != -1)
	{
	    uri.append(':');
	    uri.append(port);
	}

	assemblePath(uri, false, true, unescape);

	return uri.toString();
    }


    /**
     * @return a string representation of this URI suitable for use in
     *         links, headers, etc.
     */
    public String toExternalForm()
    {
	return stringify(false);
    }


    /**
     * Return the URI as string. This differs from toExternalForm() in that
     * all elements are unescaped before assembly. This is <em>not suitable</em>
     * for passing to other apps or in header fields and such, and is usually
     * not what you want.
     *
     * @return the URI as a string
     * @see #toExternalForm()
     */
    public String toString()
    {
	return stringify(true);
    }


    /**
     * @return true if <var>other</var> is either a URI or URL and it
     *         matches the current URI
     */
    public boolean equals(Object other)
    {
	if (other instanceof URI)
	{
	    URI o = (URI) other;
	    return (scheme.equals(o.scheme)  &&
		    (
		     type == OPAQUE  &&  areEqual(opaque, o.opaque)  ||

		     type == SEMI_GENERIC  &&
		      areEqual(userinfo, o.userinfo)  &&
		      areEqualIC(host, o.host)  &&
		      port == o.port  &&
		      areEqual(path, o.path)  ||

		     type == GENERIC  &&
		      areEqual(userinfo, o.userinfo)  &&
		      areEqualIC(host, o.host)  &&
		      port == o.port  &&
		      pathsEqual(path, o.path)  &&
		      areEqual(query, o.query)  &&
		      areEqual(fragment, o.fragment)
		    ));
	}

	if (other instanceof URL)
	{
	    URL o = (URL) other;
	    String h, f;

	    if (userinfo != null)
		h = userinfo + "@" + host;
	    else
		h = host;

	    f = getPathAndQuery();

	    return (scheme.equalsIgnoreCase(o.getProtocol())  &&
		    (type == OPAQUE  &&  opaque.equals(o.getFile())  ||

		     type == SEMI_GENERIC  &&
		       areEqualIC(h, o.getHost())  &&
		       (port == o.getPort()  ||
			o.getPort() == defaultPort(scheme))  &&
		       areEqual(f, o.getFile())  ||

		     type == GENERIC  &&
		       areEqualIC(h, o.getHost())  &&
		       (port == o.getPort()  ||
			o.getPort() == defaultPort(scheme))  &&
		       pathsEqual(f, o.getFile())  &&
		       areEqual(fragment, o.getRef())
		    )
		   );
	}

	return false;
    }

    private static final boolean areEqual(String s1, String s2)
    {
	return (s1 == null  &&  s2 == null  ||
		s1 != null  &&  s2 != null  &&
		  (s1.equals(s2)  ||
		   unescapeNoPE(s1, null).equals(unescapeNoPE(s2, null)))
	       );
    }

    private static final boolean areEqualIC(String s1, String s2)
    {
	return (s1 == null  &&  s2 == null  ||
		s1 != null  &&  s2 != null  &&
		  (s1.equalsIgnoreCase(s2)  ||
		   unescapeNoPE(s1, null).equalsIgnoreCase(unescapeNoPE(s2, null)))
	       );
    }

    private static final boolean pathsEqual(String p1, String p2)
    {
	if (p1 == null  &&  p2 == null)
	    return true;
	if (p1 == null  ||  p2 == null)
	    return false;
	if (p1.equals(p2))
	    return true;

	// ok, so it wasn't that simple. Let's split into parts and compare
	// unescaped.
	int pos1 = 0, end1 = p1.length(), pos2 = 0, end2 = p2.length();
	while (pos1 < end1  &&  pos2 < end2)
	{
	    int start1 = pos1, start2 = pos2;

	    char ch;
	    while (pos1 < end1  &&  (ch = p1.charAt(pos1)) != '/'  &&  ch != ';')
		pos1++;
	    while (pos2 < end2  &&  (ch = p2.charAt(pos2)) != '/'  &&  ch != ';')
		pos2++;

	    if (pos1 == end1  &&  pos2 < end2  ||
		pos2 == end2  &&  pos1 < end1  ||
		pos1 < end1  &&  pos2 < end2  &&  p1.charAt(pos1) != p2.charAt(pos2))
		return false;

	    if ((!p1.regionMatches(start1, p2, start2, pos1-start1)  ||  (pos1-start1) != (pos2-start2))  &&
		!unescapeNoPE(p1.substring(start1, pos1), null).equals(unescapeNoPE(p2.substring(start2, pos2), null)))
		return false;

	    pos1++;
	    pos2++;
	}

	return (pos1 == end1  &&  pos2 == end2);
    }

    private int hashCode = -1;

    /**
     * The hash code is calculated over scheme, host, path, and query.
     *
     * @return the hash code
     */
    public int hashCode()
    {
	if (hashCode == -1)
	    hashCode = (scheme != null ? unescapeNoPE(scheme, null).hashCode() : 0) + 
		       (type == OPAQUE ?
			  (opaque != null ? unescapeNoPE(opaque, null).hashCode() : 0) * 7
			: (host != null ? unescapeNoPE(host, null).toLowerCase().hashCode() : 0) * 7 +
			  (path != null ? unescapeNoPE(path, null).hashCode() : 0) * 13 +
			  (query != null ? unescapeNoPE(query, null).hashCode() : 0) * 17);

	return hashCode;
    }


    /**
     * Escape any character not in the given character class. Characters
     * greater 255 are always escaped according to ??? .
     *
     * @param elem         the string to escape
     * @param allowed_char the BitSet of all allowed characters
     * @param utf8         if true, will first UTF-8 encode unallowed characters
     * @return the string with all characters not in allowed_char
     *         escaped
     */
    public static String escape(String elem, BitSet allowed_char, boolean utf8)
    {
	return new String(escape(elem.toCharArray(), allowed_char, utf8));
    }

    /**
     * Escape any character not in the given character class. Characters
     * greater 255 are always escaped according to ??? .
     *
     * @param elem         the array of characters to escape
     * @param allowed_char the BitSet of all allowed characters
     * @param utf8         if true, will first UTF-8 encode unallowed characters
     * @return the elem array with all characters not in allowed_char
     *         escaped
     */
    public static char[] escape(char[] elem, BitSet allowed_char, boolean utf8)
    {
	int cnt=0;
	for (int idx=0; idx<elem.length; idx++)
	{
	    if (!allowed_char.get(elem[idx]))
	    {
		cnt += 2;
		if (utf8)
		{
		    if (elem[idx] >= 0x0080)
			cnt += 3;
		    if (elem[idx] >= 0x00800)
			cnt += 3;
		    if ((elem[idx] & 0xFC00) == 0xD800  &&  idx+1 < elem.length  &&
			(elem[idx+1] & 0xFC00) == 0xDC00)
		      cnt -= 6;
		}
	    }
	}

	if (cnt == 0)  return elem;

	char[] tmp = new char[elem.length + cnt];
	for (int idx=0, pos=0; idx<elem.length; idx++)
	{
	    char c = elem[idx];
	    if (allowed_char.get(c))
		tmp[pos++] = c;
	    else if (utf8)
	    {
		/* We're UTF-8 encoding the chars first, as recommended in
		 * the HTML 4.0 specification:
		 * http://www.w3.org/TR/REC-html40/appendix/notes.html#h-B.2.1
		 * Note that this doesn't change things for ASCII chars
		 */
		if (c <= 0x007F)
		{
		    pos = enc(tmp, pos, c);
		}
		else if (c <= 0x07FF)
		{
		    pos = enc(tmp, pos, 0xC0 | ((c >>  6) & 0x1F));
		    pos = enc(tmp, pos, 0x80 | ((c >>  0) & 0x3F));
		}
		else if (!((c & 0xFC00) == 0xD800  &&  idx+1 < elem.length  &&
			     (elem[idx+1] & 0xFC00) == 0xDC00))
		{
		    pos = enc(tmp, pos, 0xE0 | ((c >> 12) & 0x0F));
		    pos = enc(tmp, pos, 0x80 | ((c >>  6) & 0x3F));
		    pos = enc(tmp, pos, 0x80 | ((c >>  0) & 0x3F));
		}
		else
		{
		    int ch = ((c & 0x03FF) << 10) | (elem[++idx] & 0x03FF);
		    ch += 0x10000;
		    pos = enc(tmp, pos, 0xF0 | ((ch >> 18) & 0x07));
		    pos = enc(tmp, pos, 0x80 | ((ch >> 12) & 0x3F));
		    pos = enc(tmp, pos, 0x80 | ((ch >>  6) & 0x3F));
		    pos = enc(tmp, pos, 0x80 | ((ch >>  0) & 0x3F));
		}
	    }
	    else
		pos = enc(tmp, pos, c);
	}

	return tmp;
    }

    private static final char[] hex =
	    {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};

    private static final int enc(char[] out, int pos, int c)
    {
	out[pos++] = '%';
	out[pos++] = hex[(c >> 4) & 0xf];
	out[pos++] = hex[c & 0xf];
	return pos;
    }

    /**
     * Unescape escaped characters (i.e. %xx) except reserved ones.
     *
     * @param str      the string to unescape
     * @param reserved the characters which may not be unescaped, or null
     * @return the unescaped string
     * @exception ParseException if the two digits following a `%' are
     *            not a valid hex number
     */
    public static final String unescape(String str, BitSet reserved)
	    throws ParseException
    {
	if (str == null  ||  str.indexOf('%') == -1)
	    return str;  				// an optimization

	char[] buf = str.toCharArray();
	char[] res = new char[buf.length];

	char[] utf = new char[4];
	int utf_idx = 0, utf_len = -1;
	int didx = 0;
	for (int sidx=0; sidx<buf.length; sidx++)
	{
	    if (buf[sidx] == '%')
	    {
		int ch;
                try
                {
		    if (sidx + 3 > buf.length)
			throw new NumberFormatException();
		    ch = Integer.parseInt(str.substring(sidx+1,sidx+3), 16);
		    if (ch < 0)
			throw new NumberFormatException();
		    sidx += 2;
                }
                catch (NumberFormatException e)
                {
		    /* Hmm, people not reading specs again, so we just
		     * ignore it...
                    throw new ParseException(str.substring(sidx,sidx+3) +
                                            " is an invalid code");
		    */
		    ch = buf[sidx];
                }

		// check if we're working on a utf-char
		if (utf_len > 0)
		{
		    if ((ch & 0xC0) != 0x80)	// oops, we misinterpreted
		    {
			didx = copyBuf(utf, utf_idx, ch, res, didx, reserved, false);
			utf_len = -1;
		    }
		    else if (utf_idx == utf_len - 1)	// end-of-char
		    {
			if ((utf[0] & 0xE0) == 0xC0)
			    ch = (utf[0] & 0x1F) <<  6 |
				 (ch & 0x3F);
			else if ((utf[0] & 0xF0) == 0xE0)
			    ch = (utf[0] & 0x0F) << 12 |
				 (utf[1] & 0x3F) <<  6 |
				 (ch & 0x3F);
			else
			    ch = (utf[0] & 0x07) << 18 |
				 (utf[1] & 0x3F) << 12 |
				 (utf[2] & 0x3F) <<  6 |
				 (ch & 0x3F);
			if (reserved != null  &&  reserved.get(ch))
			    didx = copyBuf(utf, utf_idx, ch, res, didx, null, true);
			else if (utf_len < 4)
			    res[didx++] = (char) ch;
			else
			{
			    ch -= 0x10000;
			    res[didx++] = (char) ((ch >> 10)    | 0xD800);
			    res[didx++] = (char) ((ch & 0x03FF) | 0xDC00);
			}
			utf_len = -1;
		    }
		    else				// continue
			utf[utf_idx++] = (char) ch;
		}
		// check if this is the start of a utf-char
		else if ((ch & 0xE0) == 0xC0  ||  (ch & 0xF0) == 0xE0  ||
			 (ch & 0xF8) == 0xF0)
		{
		    if ((ch & 0xE0) == 0xC0)
			utf_len = 2;
		    else if ((ch & 0xF0) == 0xE0)
			utf_len = 3;
		    else
			utf_len = 4;
		    utf[0] = (char) ch;
		    utf_idx = 1;
		}
		// leave reserved alone
		else if (reserved != null  &&  reserved.get(ch))
		{
		    res[didx++] = buf[sidx];
		    sidx -= 2;
		}
		// just use the decoded version
		else
		    res[didx++] = (char) ch;
	    }
	    else if (utf_len > 0)	// oops, we misinterpreted
	    {
		didx = copyBuf(utf, utf_idx, buf[sidx], res, didx, reserved, false);
		utf_len = -1;
	    }
	    else
		res[didx++] = buf[sidx];
	}
	if (utf_len > 0)	// oops, we misinterpreted
	    didx = copyBuf(utf, utf_idx, -1, res, didx, reserved, false);

	return new String(res, 0, didx);
    }

    private static final int copyBuf(char[] utf, int utf_idx, int ch,
				     char[] res, int didx, BitSet reserved,
				     boolean escapeAll)
    {
	if (ch >= 0)
	    utf[utf_idx++] = (char) ch;

	for (int idx=0; idx<utf_idx; idx++)
	{
	    if (reserved != null  &&  reserved.get(utf[idx])  ||  escapeAll)
		didx = enc(res, didx, utf[idx]);
	    else
		res[didx++] = utf[idx];
	}

	return didx;
    }

    /**
     * Unescape escaped characters (i.e. %xx). If a ParseException would
     * be thrown then just return the original string.
     *
     * @param str      the string to unescape
     * @param reserved the characters which may not be unescaped, or null
     * @return the unescaped string, or the original string if unescaping
     *         would throw a ParseException
     * @see #unescape(java.lang.String, java.util.BitSet)
     */
    private static final String unescapeNoPE(String str, BitSet reserved)
    {
	try
	    { return unescape(str, reserved); }
	catch (ParseException pe)
	    { return str; }
    }


    /**
     * Run test set.
     *
     * @exception Exception if any test fails
     */
    public static void main(String args[])  throws Exception
    {
	System.err.println();
	System.err.println("*** URI Tests ...");


	/* Relative URI test set, taken from Section C of rfc-2396 and
	 * Roy's test1. All Roy's URI parser tests can be found at
	 * http://www.ics.uci.edu/~fielding/url/
	 * The tests have been augmented by a few for the IPv6 syntax
	 */

	URI base = new URI("http://a/b/c/d;p?q");

	// normal examples
	testParser(base, "g:h",        "g:h");
	testParser(base, "g",          "http://a/b/c/g");
	testParser(base, "./g",        "http://a/b/c/g");
	testParser(base, "g/",         "http://a/b/c/g/");
	testParser(base, "/g",         "http://a/g");
	testParser(base, "//g",        "http://g");
	testParser(base, "//[23:54]",  "http://[23:54]");
	testParser(base, "?y",         "http://a/b/c/?y");
	testParser(base, "g?y",        "http://a/b/c/g?y");
	testParser(base, "#s",         "http://a/b/c/d;p?q#s");
	testParser(base, "g#s",        "http://a/b/c/g#s");
	testParser(base, "g?y#s",      "http://a/b/c/g?y#s");
	testParser(base, ";x",         "http://a/b/c/;x");
	testParser(base, "g;x",        "http://a/b/c/g;x");
	testParser(base, "g;x?y#s",    "http://a/b/c/g;x?y#s");
	testParser(base, ".",          "http://a/b/c/");
	testParser(base, "./",         "http://a/b/c/");
	testParser(base, "..",         "http://a/b/");
	testParser(base, "../",        "http://a/b/");
	testParser(base, "../g",       "http://a/b/g");
	testParser(base, "../..",      "http://a/");
	testParser(base, "../../",     "http://a/");
	testParser(base, "../../g",    "http://a/g");

	// abnormal examples
	testParser(base, "",              "http://a/b/c/d;p?q");
	testParser(base, "/./g",          "http://a/./g");
	testParser(base, "/../g",         "http://a/../g");
	testParser(base, "../../../g",    "http://a/../g");
	testParser(base, "../../../../g", "http://a/../../g");
	testParser(base, "g.",            "http://a/b/c/g.");
	testParser(base, ".g",            "http://a/b/c/.g");
	testParser(base, "g..",           "http://a/b/c/g..");
	testParser(base, "..g",           "http://a/b/c/..g");
	testParser(base, "./../g",        "http://a/b/g");
	testParser(base, "./g/.",         "http://a/b/c/g/");
	testParser(base, "g/./h",         "http://a/b/c/g/h");
	testParser(base, "g/../h",        "http://a/b/c/h");
	testParser(base, "g;x=1/./y",     "http://a/b/c/g;x=1/y");
	testParser(base, "g;x=1/../y",    "http://a/b/c/y");
	testParser(base, "g?y/./x",       "http://a/b/c/g?y/./x");
	testParser(base, "g?y/../x",      "http://a/b/c/g?y/../x");
	testParser(base, "g#s/./x",       "http://a/b/c/g#s/./x");
	testParser(base, "g#s/../x",      "http://a/b/c/g#s/../x");
	if (ENABLE_BACKWARDS_COMPATIBILITY)
	    testParser(base, "http:g",        "http://a/b/c/g");
	else
	    testParser(base, "http:g",        "http:g");
	if (ENABLE_BACKWARDS_COMPATIBILITY)
	    testParser(base, "http:",         "http://a/b/c/d;p?q");
	else
	    testParser(base, "http:",         "http:");
	testParser(base, "./g:h",         "http://a/b/c/g:h");


	/* Roy's test2
	 */
	base = new URI("http://a/b/c/d;p?q=1/2");

	testParser(base, "g",        "http://a/b/c/g");
	testParser(base, "./g",      "http://a/b/c/g");
	testParser(base, "g/",       "http://a/b/c/g/");
	testParser(base, "/g",       "http://a/g");
	testParser(base, "//g",      "http://g");
	testParser(base, "//[23:54]","http://[23:54]");
	testParser(base, "?y",       "http://a/b/c/?y");
	testParser(base, "g?y",      "http://a/b/c/g?y");
	testParser(base, "g?y/./x",  "http://a/b/c/g?y/./x");
	testParser(base, "g?y/../x", "http://a/b/c/g?y/../x");
	testParser(base, "g#s",      "http://a/b/c/g#s");
	testParser(base, "g#s/./x",  "http://a/b/c/g#s/./x");
	testParser(base, "g#s/../x", "http://a/b/c/g#s/../x");
	testParser(base, "./",       "http://a/b/c/");
	testParser(base, "../",      "http://a/b/");
	testParser(base, "../g",     "http://a/b/g");
	testParser(base, "../../",   "http://a/");
	testParser(base, "../../g",  "http://a/g");


	/* Roy's test3
	 */
	base = new URI("http://a/b/c/d;p=1/2?q");

	testParser(base, "g",          "http://a/b/c/d;p=1/g");
	testParser(base, "./g",        "http://a/b/c/d;p=1/g");
	testParser(base, "g/",         "http://a/b/c/d;p=1/g/");
	testParser(base, "g?y",        "http://a/b/c/d;p=1/g?y");
	testParser(base, ";x",         "http://a/b/c/d;p=1/;x");
	testParser(base, "g;x",        "http://a/b/c/d;p=1/g;x");
	testParser(base, "g;x=1/./y",  "http://a/b/c/d;p=1/g;x=1/y");
	testParser(base, "g;x=1/../y", "http://a/b/c/d;p=1/y");
	testParser(base, "./",         "http://a/b/c/d;p=1/");
	testParser(base, "../",        "http://a/b/c/");
	testParser(base, "../g",       "http://a/b/c/g");
	testParser(base, "../../",     "http://a/b/");
	testParser(base, "../../g",    "http://a/b/g");


	/* Roy's test4
	 */
	base = new URI("fred:///s//a/b/c");

	testParser(base, "g:h",           "g:h");
	/* we have to skip these, as usesGeneraicSyntax("fred") returns false
	 * and we therefore don't parse relative URI's here. But test5 is
	 * the same except that the http scheme is used.
	testParser(base, "g",             "fred:///s//a/b/g");
	testParser(base, "./g",           "fred:///s//a/b/g");
	testParser(base, "g/",            "fred:///s//a/b/g/");
	testParser(base, "/g",            "fred:///g");
	testParser(base, "//g",           "fred://g");
	testParser(base, "//g/x",         "fred://g/x");
	testParser(base, "///g",          "fred:///g");
	testParser(base, "./",            "fred:///s//a/b/");
	testParser(base, "../",           "fred:///s//a/");
	testParser(base, "../g",          "fred:///s//a/g");
	testParser(base, "../../",        "fred:///s//");
	testParser(base, "../../g",       "fred:///s//g");
	testParser(base, "../../../g",    "fred:///s/g");
	testParser(base, "../../../../g", "fred:///g");
	 */
	testPE(base, "g");


	/* Roy's test5
	 */
	base = new URI("http:///s//a/b/c");

	testParser(base, "g:h",           "g:h");
	testParser(base, "g",             "http:///s//a/b/g");
	testParser(base, "./g",           "http:///s//a/b/g");
	testParser(base, "g/",            "http:///s//a/b/g/");
	testParser(base, "/g",            "http:///g");
	testParser(base, "//g",           "http://g");
	testParser(base, "//[23:54]",     "http://[23:54]");
	testParser(base, "//g/x",         "http://g/x");
	testParser(base, "///g",          "http:///g");
	testParser(base, "./",            "http:///s//a/b/");
	testParser(base, "../",           "http:///s//a/");
	testParser(base, "../g",          "http:///s//a/g");
	testParser(base, "../../",        "http:///s//");
	testParser(base, "../../g",       "http:///s//g");
	testParser(base, "../../../g",    "http:///s/g");
	testParser(base, "../../../../g", "http:///g");


	/* Some additional parser tests
	 */
	base = new URI("http://s");

	testParser(base, "ftp:h",         "ftp:h");
	testParser(base, "ftp://h",       "ftp://h");
	testParser(base, "//g",           "http://g");
	testParser(base, "//g?h",         "http://g?h");
	testParser(base, "g",             "http://s/g");
	testParser(base, "./g",           "http://s/g");
	testParser(base, "?g",            "http://s/?g");
	testParser(base, "#g",            "http://s#g");

	base = new URI("http:");

	testParser(base, "ftp:h",         "ftp:h");
	testParser(base, "ftp://h",       "ftp://h");
	testParser(base, "//g",           "http://g");
	testParser(base, "g",             "http:/g");
	testParser(base, "?g",            "http:/?g");
	testParser(base, "#g",            "http:#g");

	base = new URI("http://s/t");

	testParser(base, "ftp:/h",        "ftp:/h");
	if (ENABLE_BACKWARDS_COMPATIBILITY)
	    testParser(base, "http:/h",       "http://s/h");
	else
	    testParser(base, "http:/h",       "http:/h");

	base = new URI("http://s/g?h/j");
	testParser(base, "k",             "http://s/k");
	testParser(base, "k?l",           "http://s/k?l");


	/* Parser tests for semi-generic syntax
	 */
	base = new URI("ldap:");

	testParser(base, "ldap:",         "ldap:");
	testParser(base, "ldap://a",      "ldap://a");
	testParser(base, "ldap://a/b",    "ldap://a/b");
	testParser(base, "ldap:/b",       "ldap:/b");

	testParser(base, "ftp:h",         "ftp:h");
	testParser(base, "ftp://h",       "ftp://h");
	testParser(base, "//g",           "ldap://g");
	testParser(base, "//g?h",         "ldap://g/?h");
	testParser(base, "g",             "ldap:/g");
	testParser(base, "./g",           "ldap:/./g");
	testParser(base, "?g",            "ldap:/?g");
	testParser(base, "#g",            "ldap:/%23g");

	base = new URI("ldap://s");

	if (ENABLE_BACKWARDS_COMPATIBILITY)
	    testParser(base, "ldap:",         "ldap://s");
	else
	    testParser(base, "ldap:",         "ldap:");
	testParser(base, "ldap://a",      "ldap://a");
	testParser(base, "ldap://a/b",    "ldap://a/b");
	if (ENABLE_BACKWARDS_COMPATIBILITY)
	    testParser(base, "ldap:/b",       "ldap://s/b");
	else
	    testParser(base, "ldap:/b",       "ldap:/b");

	testParser(base, "ftp:h",         "ftp:h");
	testParser(base, "ftp://h",       "ftp://h");
	testParser(base, "//g",           "ldap://g");
	testParser(base, "//g?h",         "ldap://g/?h");
	testParser(base, "g",             "ldap://s/g");
	testParser(base, "./g",           "ldap://s/./g");
	testParser(base, "?g",            "ldap://s/?g");
	testParser(base, "#g",            "ldap://s/%23g");

	base = new URI("ldap://s/t");

	testParser(base, "ftp:/h",        "ftp:/h");
	if (ENABLE_BACKWARDS_COMPATIBILITY)
	    testParser(base, "ldap:/h",       "ldap://s/h");
	else
	    testParser(base, "ldap:/h",       "ldap:/h");

	if (ENABLE_BACKWARDS_COMPATIBILITY)
	    testParser(base, "ldap:",         "ldap://s");
	else
	    testParser(base, "ldap:",         "ldap:");
	testParser(base, "ldap://a",      "ldap://a");
	testParser(base, "ldap://a/b",    "ldap://a/b");

	testParser(base, "ftp:h",         "ftp:h");
	testParser(base, "ftp://h",       "ftp://h");
	testParser(base, "//g",           "ldap://g");
	testParser(base, "//g?h",         "ldap://g/?h");
	testParser(base, "g",             "ldap://s/g");
	testParser(base, "./g",           "ldap://s/./g");
	testParser(base, "?g",            "ldap://s/?g");
	testParser(base, "#g",            "ldap://s/%23g");


	/* equality tests */

	// protocol
	testNotEqual("http://a/", "nntp://a/");
	testNotEqual("http://a/", "https://a/");
	testNotEqual("http://a/", "shttp://a/");
	testEqual("http://a/", "Http://a/");
	testEqual("http://a/", "hTTP://a/");
	testEqual("url:http://a/", "hTTP://a/");
	testEqual("urI:http://a/", "hTTP://a/");

	// host
	testEqual("http://a/", "Http://A/");
	testEqual("http://a.b.c/", "Http://A.b.C/");
	testEqual("http:///", "Http:///");
	testEqual("http://[]/", "Http:///");
	testNotEqual("http:///", "Http://a/");
	testNotEqual("http://[]/", "Http://a/");
	testPE(null, "ftp://[23::43:1/");
	testPE(null, "ftp://[/");

	// port
	testEqual("http://a.b.c/", "Http://A.b.C:80/");
	testEqual("http://a.b.c:/", "Http://A.b.C:80/");
	testEqual("http://[23::45:::5:]/", "Http://[23::45:::5:]:80/");
	testEqual("http://[23::45:::5:]:/", "Http://[23::45:::5:]:80/");
	testEqual("nntp://a", "nntp://a:119");
	testEqual("nntp://a:", "nntp://a:119");
	testEqual("nntp://a/", "nntp://a:119/");
	testNotEqual("nntp://a", "nntp://a:118");
	testNotEqual("nntp://a", "nntp://a:0");
	testNotEqual("nntp://a:", "nntp://a:0");
	testEqual("telnet://:23/", "telnet:///");
	testPE(null, "ftp://:a/");
	testPE(null, "ftp://:-1/");
	testPE(null, "ftp://::1/");

	// userinfo
	testNotEqual("ftp://me@a", "ftp://a");
	testNotEqual("ftp://me@a", "ftp://Me@a");
	testEqual("ftp://Me@a", "ftp://Me@a");
	testEqual("ftp://Me:My@a:21", "ftp://Me:My@a");
	testEqual("ftp://Me:My@a:", "ftp://Me:My@a");
	testNotEqual("ftp://Me:My@a:21", "ftp://Me:my@a");
	testNotEqual("ftp://Me:My@a:", "ftp://Me:my@a");

	// path
	testEqual("ftp://a/b%2b/", "ftp://a/b+/");
	testEqual("ftp://a/b%2b/", "ftp://a/b+/");
	testEqual("ftp://a/b%5E/", "ftp://a/b^/");
	testEqual("ftp://a/b%4C/", "ftp://a/bL/");
	testNotEqual("ftp://a/b/", "ftp://a//b/");
	testNotEqual("ftp://a/b/", "ftp://a/b//");
	testNotEqual("ftp://a/b%4C/", "ftp://a/bl/");
	testNotEqual("ftp://a/b%3f/", "ftp://a/b?/");
	testNotEqual("ftp://a/b%2f/", "ftp://a/b//");
	testNotEqual("ftp://a/b%2fc/", "ftp://a/b/c/");
	testNotEqual("ftp://a/bc/", "ftp://a/b//");
	testNotEqual("ftp://a/bc/", "ftp://a/b/");
	testNotEqual("ftp://a/bc//", "ftp://a/b/");
	testNotEqual("ftp://a/b/", "ftp://a/bc//");
	testNotEqual("ftp://a/b/", "ftp://a/bc/");
	testNotEqual("ftp://a/b//", "ftp://a/bc/");

	testNotEqual("ftp://a/b;fc/", "ftp://a/bf;c/");
	testNotEqual("ftp://a/b%3bfc/", "ftp://a/b;fc/");
	testEqual("ftp://a/b;/;/", "ftp://a/b;/;/");
	testNotEqual("ftp://a/b;/", "ftp://a/b//");
	testNotEqual("ftp://a/b//", "ftp://a/b;/");
	testNotEqual("ftp://a/b/;", "ftp://a/b//");
	testNotEqual("ftp://a/b//", "ftp://a/b/;");
	testNotEqual("ftp://a/b;/", "ftp://a/b;//");
	testNotEqual("ftp://a/b;//", "ftp://a/b;/");

	// escaping/unescaping
	testEscape("hello\u1212there", "hello%E1%88%92there");
	testEscape("hello\u0232there", "hello%C8%B2there");
	testEscape("hello\uDA42\uDD42there", "hello%F2%A0%A5%82there");
	testEscape("hello\uDA42", "hello%ED%A9%82");
	testEscape("hello\uDA42there", "hello%ED%A9%82there");
	testUnescape("hello%F2%A0%A5%82there", "hello\uDA42\uDD42there");
	testUnescape("hello%F2%A0%A5there", "hello\u00F2\u00A0\u00A5there");
	testUnescape("hello%F2%A0there", "hello\u00F2\u00A0there");
	testUnescape("hello%F2there", "hello\u00F2there");
	testUnescape("hello%F2%A0%A5%82", "hello\uDA42\uDD42");
	testUnescape("hello%F2%A0%A5", "hello\u00F2\u00A0\u00A5");
	testUnescape("hello%F2%A0", "hello\u00F2\u00A0");
	testUnescape("hello%F2", "hello\u00F2");
	testUnescape("hello%E1%88%92there", "hello\u1212there");
	testUnescape("hello%E1%88there", "hello\u00E1\u0088there");
	testUnescape("hello%E1there", "hello\u00E1there");
	testUnescape("hello%E1%71there", "hello\u00E1qthere");
	testUnescape("hello%E1%88", "hello\u00E1\u0088");
	testUnescape("hello%E1%71", "hello\u00E1q");
	testUnescape("hello%E1", "hello\u00E1");
	testUnescape("hello%C8%B2there", "hello\u0232there");
	testUnescape("hello%C8there", "hello\u00C8there");
	testUnescape("hello%C8%71there", "hello\u00C8qthere");
	testUnescape("hello%C8%71", "hello\u00C8q");
	testUnescape("hello%C8", "hello\u00C8");
	testUnescape("%71there", "qthere");
	testUnescape("%B1there", "\u00B1there");

	System.err.println("*** Tests finished successfuly");
    }

    private static final String nl = System.getProperty("line.separator");

    private static void testParser(URI base, String relURI, String result)
	    throws Exception
    {
	if (!(new URI(base, relURI).toExternalForm().equals(result)))
	{
	    throw new Exception("Test failed: " + nl +
				"  base-URI = <" + base + ">" + nl +
				"  rel-URI  = <" + relURI + ">" + nl+
				"  expected   <" + result + ">" + nl+
				"  but got    <" + new URI(base, relURI) + ">");
	}
    }

    private static void testEqual(String one, String two)  throws Exception
    {
	URI u1 = new URI(one);
	URI u2 = new URI(two);

	if (!u1.equals(u2))
	{
	    throw new Exception("Test failed: " + nl +
				"  <" + one + "> != <" + two + ">");
	}
	if (u1.hashCode() != u2.hashCode())
	{
	    throw new Exception("Test failed: " + nl +
				"  hashCode <" + one + "> != hashCode <" + two + ">");
	}
    }

    private static void testNotEqual(String one, String two)  throws Exception
    {
	URI u1 = new URI(one);
	URI u2 = new URI(two);

	if (u1.equals(u2))
	{
	    throw new Exception("Test failed: " + nl +
				"  <" + one + "> == <" + two + ">");
	}
    }

    private static void testPE(URI base, String uri)  throws Exception
    {
	boolean got_pe = false;
	try
	    { new URI(base, uri); }
	catch (ParseException pe)
	    { got_pe = true; }
	if (!got_pe)
	{
	    throw new Exception("Test failed: " + nl +
				"  <" + uri + "> should be invalid");
	}
    }

    private static void testEscape(String raw, String escaped)  throws Exception
    {
	String test = new String(escape(raw.toCharArray(), uricChar, true));
	if (!test.equals(escaped))
	    throw new Exception("Test failed: " + nl +
				"  raw-string: " + raw + nl +
				"  escaped:    " + test + nl +
				"  expected:   " + escaped);
    }

    private static void testUnescape(String escaped, String raw)
	throws Exception
    {
	if (!unescape(escaped, null).equals(raw))
	    throw new Exception("Test failed: " + nl +
				"  escaped-string: " + escaped + nl +
				"  unescaped:      " + unescape(escaped, null) + nl +
				"  expected:       " + raw);
    }
}
