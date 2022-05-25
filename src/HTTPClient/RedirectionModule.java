/*
 * @(#)RedirectionModule.java				0.3-3 06/05/2001
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

import java.net.ProtocolException;
import java.io.IOException;
import java.util.Hashtable;


/**
 * This module handles the redirection status codes 301, 302, 303, 305, 306
 * and 307.
 *
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 */
class RedirectionModule implements HTTPClientModule
{
    /** a list of permanent redirections (301) */
    private static Hashtable perm_redir_cntxt_list = new Hashtable();

    /** a list of deferred redirections (used with Response.retryRequest()) */
    private static Hashtable deferred_redir_list = new Hashtable();

    /** the level of redirection */
    private int level;

    /** the url used in the last redirection */
    private URI lastURI;

    /** used for deferred redirection retries */
    private boolean new_con;

    /** used for deferred redirection retries */
    private Request saved_req;


    // Constructors

    /**
     * Start with level 0.
     */
    RedirectionModule()
    {
	level     = 0;
	lastURI   = null;
	saved_req = null;
    }


    // Methods

    /**
     * Invoked by the HTTPClient.
     */
    public int requestHandler(Request req, Response[] resp)
    {
	HTTPConnection con = req.getConnection();
	URI new_loc,
	    cur_loc;


	// check for retries

	HttpOutputStream out = req.getStream();
	if (out != null  &&  deferred_redir_list.get(out) != null)
	{
	    copyFrom((RedirectionModule) deferred_redir_list.remove(out));
	    req.copyFrom(saved_req);

	    if (new_con)
		return REQ_NEWCON_RST;
	    else
		return REQ_RESTART;
	}


	// handle permanent redirections

	try
	{
	    cur_loc = new URI(new URI(con.getProtocol(), con.getHost(), con.getPort(), null),
			      req.getRequestURI());
	}
	catch (ParseException pe)
	{
	    throw new Error("HTTPClient Internal Error: unexpected exception '"
			    + pe + "'");
	}


	// handle permanent redirections

	Hashtable perm_redir_list = Util.getList(perm_redir_cntxt_list,
					    req.getConnection().getContext());
	if ((new_loc = (URI) perm_redir_list.get(cur_loc)) != null)
	{
	    /* copy query if present in old url but not in new url. This
	     * isn't strictly conforming, but some scripts fail to properly
	     * propagate the query string to the Location header.
	     *
	     * Unfortunately it looks like we're fucked either way: some
	     * scripts fail if you don't propagate the query string, some
	     * fail if you do... God, don't you just love it when people
	     * can't read a spec? Anway, since we can't get it right for
	     * all scripts we opt to follow the spec.
	    String nres    = new_loc.getPathAndQuery(),
		   oquery  = Util.getQuery(req.getRequestURI()),
		   nquery  = Util.getQuery(nres);
	    if (nquery == null  &&  oquery != null)
		nres += "?" + oquery;
	     */
	    String nres = new_loc.getPathAndQuery();
	    req.setRequestURI(nres);

	    try
		{ lastURI = new URI(new_loc, nres); }
	    catch (ParseException pe)
		{ }

	    Log.write(Log.MODS, "RdirM: matched request in permanent " +
				"redirection list - redoing request to " +
				lastURI.toExternalForm());

	    if (!con.isCompatibleWith(new_loc))
	    {
		try
		    { con = new HTTPConnection(new_loc); }
		catch (Exception e)
		{
		    throw new Error("HTTPClient Internal Error: unexpected " +
				    "exception '" + e + "'");
		}

		con.setContext(req.getConnection().getContext());
		req.setConnection(con);
		return REQ_NEWCON_RST;
	    }
	    else
	    {
		return REQ_RESTART;
	    }
	}

	return REQ_CONTINUE;
    }


    /**
     * Invoked by the HTTPClient.
     */
    public void responsePhase1Handler(Response resp, RoRequest req)
	    throws IOException
    {
	int sts  = resp.getStatusCode();
	if (sts < 301  ||  sts > 307  ||  sts == 304)
	{
	    if (lastURI != null)		// it's been redirected
		resp.setEffectiveURI(lastURI);
	}
    }


    /**
     * Invoked by the HTTPClient.
     */
    public int responsePhase2Handler(Response resp, Request req)
	    throws IOException
    {
	/* handle various response status codes until satisfied */

	int sts  = resp.getStatusCode();
	switch(sts)
	{
	    case 302: // General (temporary) Redirection (handle like 303)

		/* Note we only do this munging for POST and PUT. For GET it's
		 * not necessary; for HEAD we probably want to do another HEAD.
		 * For all others (i.e. methods from WebDAV, IPP, etc) it's
		 * somewhat unclear - servers supporting those should really
		 * return a 307 or 303, but some don't (guess who...), so we
		 * just don't touch those.
		 */
		if (req.getMethod().equals("POST")  ||
		    req.getMethod().equals("PUT"))
		{
		    Log.write(Log.MODS, "RdirM: Received status: " + sts +
					" " + resp.getReasonLine() +
					" - treating as 303");

		    sts = 303;
		}

	    case 301: // Moved Permanently
	    case 303: // See Other (use GET)
	    case 307: // Moved Temporarily (we mean it!)

		Log.write(Log.MODS, "RdirM: Handling status: " + sts +
				    " " + resp.getReasonLine());

		// the spec says automatic redirection may only be done if
		// the second request is a HEAD or GET.
		if (!req.getMethod().equals("GET")  &&
		    !req.getMethod().equals("HEAD")  &&
		    sts != 303)
		{
		    Log.write(Log.MODS, "RdirM: not redirected because " +
					"method is neither HEAD nor GET");

		    if (sts == 301  &&  resp.getHeader("Location") != null)
			update_perm_redir_list(req,
				    resLocHdr(resp.getHeader("Location"), req));

		    resp.setEffectiveURI(lastURI);
		    return RSP_CONTINUE;
		}

	    case 305: // Use Proxy
	    case 306: // Switch Proxy

		if (sts == 305  ||  sts == 306)
		    Log.write(Log.MODS, "RdirM: Handling status: " + sts +
				        " " + resp.getReasonLine());

		// Don't accept 305 from a proxy
		if (sts == 305  &&  req.getConnection().getProxyHost() != null)
		{
		    Log.write(Log.MODS, "RdirM: 305 ignored because " +
					"a proxy is already in use");

		    resp.setEffectiveURI(lastURI);
		    return RSP_CONTINUE;
		}


		/* the level is a primitive way of preventing infinite
		 * redirections. RFC-2068 set the max to 5, but RFC-2616
		 * has loosened this. Since some sites (notably M$) need
		 * more levels, this is now set to the (arbitrary) value
		 * of 15 (god only knows why they need to do even 5
		 * redirections...).
		 */
		if (level >= 15  ||  resp.getHeader("Location") == null)
		{
		    if (level >= 15)
			Log.write(Log.MODS, "RdirM: not redirected because "+
					    "of too many levels of redirection");
		    else
			Log.write(Log.MODS, "RdirM: not redirected because "+
					    "no Location header was present");

		    resp.setEffectiveURI(lastURI);
		    return RSP_CONTINUE;
		}
		level++;

		URI loc = resLocHdr(resp.getHeader("Location"), req);

		HTTPConnection mvd;
		String nres;
		new_con = false;

		if (sts == 305)
		{
		    mvd = new HTTPConnection(req.getConnection().getProtocol(),
					     req.getConnection().getHost(),
					     req.getConnection().getPort());
		    mvd.setCurrentProxy(loc.getHost(), loc.getPort());
		    mvd.setContext(req.getConnection().getContext());
		    new_con = true;

		    nres = req.getRequestURI();

		    /* There was some discussion about this, and especially
		     * Foteos Macrides (Lynx) said a 305 should also imply
		     * a change to GET (for security reasons) - see the thread
		     * starting at
		     * http://www.ics.uci.edu/pub/ietf/http/hypermail/1997q4/0351.html
		     * However, this is not in the latest draft, but since I
		     * agree with Foteos we do it anyway...
		     */
		    req.setMethod("GET");
		    req.setData(null);
		    req.setStream(null);
		}
		else if (sts == 306)
		{
		    // We'll have to wait for Josh to create a new spec here.
		    return RSP_CONTINUE;
		}
		else
		{
		    if (req.getConnection().isCompatibleWith(loc))
		    {
			mvd  = req.getConnection();
			nres = loc.getPathAndQuery();
		    }
		    else
		    {
			try
			{
			    mvd  = new HTTPConnection(loc);
			    nres = loc.getPathAndQuery();
			}
			catch (Exception e)
			{
			    if (req.getConnection().getProxyHost() == null  ||
				!loc.getScheme().equalsIgnoreCase("ftp"))
				return RSP_CONTINUE;

			    // We're using a proxy and the protocol is ftp -
			    // maybe the proxy will also proxy ftp...
			    mvd  = new HTTPConnection("http",
					    req.getConnection().getProxyHost(),
					    req.getConnection().getProxyPort());
			    mvd.setCurrentProxy(null, 0);
			    nres = loc.toExternalForm();
			}

			mvd.setContext(req.getConnection().getContext());
			new_con = true;
		    }

		    /* copy query if present in old url but not in new url.
		     * This isn't strictly conforming, but some scripts fail
		     * to propagate the query properly to the Location
		     * header.
		     *
		     * See comment on line 126.
		    String oquery  = Util.getQuery(req.getRequestURI()),
			   nquery  = Util.getQuery(nres);
		    if (nquery == null  &&  oquery != null)
			nres += "?" + oquery;
		     */

		    if (sts == 303)
		    {
			// 303 means "use GET"

			if (!req.getMethod().equals("HEAD"))
			    req.setMethod("GET");
			req.setData(null);
			req.setStream(null);
		    }
		    else
		    {
			// If they used an output stream then they'll have
			// to do the resend themselves
			if (req.getStream() != null)
			{
			    if (!HTTPConnection.deferStreamed)
			    {
				Log.write(Log.MODS, "RdirM: status " + sts +
						    " not handled - request " +
						    "has an output stream");
				return RSP_CONTINUE;
			    }

			    saved_req = (Request) req.clone();
			    deferred_redir_list.put(req.getStream(), this);
			    req.getStream().reset();
			    resp.setRetryRequest(true);
			}

			if (sts == 301)
			{
			    // update permanent redirection list
			    try
			    {
				update_perm_redir_list(req, new URI(loc, nres));
			    }
			    catch (ParseException pe)
			    {
				throw new Error("HTTPClient Internal Error: " +
						"unexpected exception '" + pe +
						"'");
			    }
			}
		    }

		    // Adjust Referer, if present
		    NVPair[] hdrs = req.getHeaders();
		    for (int idx=0; idx<hdrs.length; idx++)
			if (hdrs[idx].getName().equalsIgnoreCase("Referer"))
			{
			    HTTPConnection con = req.getConnection();
			    hdrs[idx] =
				new NVPair("Referer", con+req.getRequestURI());
			    break;
			}
		}

		req.setConnection(mvd);
		req.setRequestURI(nres);

		try { resp.getInputStream().close(); }
		catch (IOException ioe) { }

		if (sts != 305  &&  sts != 306)
		{
		    try
			{ lastURI = new URI(loc, nres); }
		    catch (ParseException pe)
			{ /* ??? */ }

		    Log.write(Log.MODS, "RdirM: request redirected to " +
					lastURI.toExternalForm() +
					" using method " + req.getMethod());
		}
		else
		{
		    Log.write(Log.MODS, "RdirM: resending request using " +
					"proxy " + mvd.getProxyHost() +
					":" + mvd.getProxyPort());
		}

		if (req.getStream() != null)
		    return RSP_CONTINUE;
		else if (new_con)
		    return RSP_NEWCON_REQ;
		else
		    return RSP_REQUEST;

	    default:

		return RSP_CONTINUE;
	}
    }


    /**
     * Invoked by the HTTPClient.
     */
    public void responsePhase3Handler(Response resp, RoRequest req)
    {
    }


    /**
     * Invoked by the HTTPClient.
     */
    public void trailerHandler(Response resp, RoRequest req)
    {
    }


    /**
     * Update the permanent redirection list.
     *
     * @param the original request
     * @param the new location
     */
    private static void update_perm_redir_list(RoRequest req, URI new_loc)
    {
	HTTPConnection con = req.getConnection();
	URI cur_loc = null;
	try
	{
	    cur_loc = new URI(new URI(con.getProtocol(), con.getHost(), con.getPort(), null),
			      req.getRequestURI());
	}
	catch (ParseException pe)
	    { }

	if (!cur_loc.equals(new_loc))
	{
	    Hashtable perm_redir_list =
			Util.getList(perm_redir_cntxt_list, con.getContext());
	    perm_redir_list.put(cur_loc, new_loc);
	}
    }


    /**
     * The Location header field must be an absolute URI, but too many broken
     * servers use relative URIs. So, we always resolve relative to the
     * full request URI.
     *
     * @param  loc the Location header field
     * @param  req the Request to resolve relative URI's relative to
     * @return an absolute URI corresponding to the Location header field
     * @exception ProtocolException if the Location header field is completely
     *                            unparseable
     */
    private URI resLocHdr(String loc, RoRequest req)  throws ProtocolException
    {
	try
	{
	    URI base = new URI(req.getConnection().getProtocol(),
			       req.getConnection().getHost(),
			       req.getConnection().getPort(), null);
	    base = new URI(base, req.getRequestURI());
	    URI res = new URI(base, loc);
	    if (res.getHost() == null)
		throw new ProtocolException("Malformed URL in Location header: `" + loc +
					    "' - missing host field");
	    return res;
	}
	catch (ParseException pe)
	{
	    throw new ProtocolException("Malformed URL in Location header: `" + loc +
					"' - exception was: " + pe.getMessage());
	}
    }


    private void copyFrom(RedirectionModule other)
    {
	this.level     = other.level;
	this.lastURI   = other.lastURI;
	this.saved_req = other.saved_req;
    }
}
