/*
 * @(#)IdempotentSequence.java				0.3-3 06/05/2001
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

//import it.unitn.ing.rista.util.Misc;

import java.util.Hashtable;
import java.util.Enumeration;

/**
 * <P>This class checks whether a sequence of requests is idempotent. This
 * is used to determine which requests may be automatically retried. This
 * class also serves as a central place to record which methods have side
 * effects and which methods are idempotent.
 *
 * <P>Note: unknown methods (i.e. a method which is not HEAD, GET, POST, PUT,
 * DELETE, OPTIONS, TRACE, PROPFIND, PROPPATCH, MKCOL, COPY, MOVE, LOCK, or
 * UNLOCK) are treated conservatively, meaning they are assumed to have side
 * effects and are not idempotent.
 *
 * <P>Usage:
 * <PRE>
 *     IdempotentSequence seq = new IdempotentSequence();
 *     seq.add(r1);
 *     ...
 *     if (seq.isIdempotent(r1)) ...
 *     ...
 * </PRE>
 *
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 */
class IdempotentSequence
{
    /** method number definitions */
    private static final int UNKNOWN = 0,
			     HEAD    = 1,
			     GET     = 2,
			     POST    = 3,
			     PUT     = 4,
			     DELETE  = 5,
			     OPTIONS = 6,
			     TRACE   = 7,

			     // DAV methods
			     PROPFIND  = 8,
			     PROPPATCH = 9,
			     MKCOL     = 10,
			     COPY      = 11,
			     MOVE      = 12,
			     LOCK      = 13,
			     UNLOCK    = 14;

    /** these are the history of previous requests */
    private int[]    m_history;
    private String[] r_history;
    private int      m_len, r_len;

    /** trigger analysis of threads */
    private boolean   analysis_done = false;
    private Hashtable threads = new Hashtable();


    // Constructors

    /**
     * Start a new sequence of requests.
     */
    public IdempotentSequence()
    {
	m_history = new int[10];
	r_history = new String[10];
	m_len = 0;
	r_len = 0;
    }


    // Methods

    /**
     * Add the request to the end of the list of requests. This is used
     * to build the complete sequence of requests before determining
     * whether the sequence is idempotent.
     *
     * @param req the next request
     */
    public void add(Request req)
    {
	if (m_len >= m_history.length)
	    m_history = Util.resizeArray(m_history, m_history.length+10);
	m_history[m_len++] = methodNum(req.getMethod());

	if (r_len >= r_history.length)
	    r_history = Util.resizeArray(r_history, r_history.length+10);
	r_history[r_len++] = req.getRequestURI();
    }


    /**
     * Is this request part of an idempotent sequence? This method <em>must
     * not</em> be called before all requests have been added to this
     * sequence; similarly, <var>add()</var> <em>must not</em> be called
     * after this method was invoked.
     *
     * <P>We split up the sequence of requests into individual sub-sequences,
     * or threads, with all requests in a thread having the same request-URI
     * and no two threads having the same request-URI. Each thread is then
     * marked as idempotent or not according to the following rules:
     *
     * <OL>
     * <LI>If any method is UNKNOWN then the thread is not idempotent;
     * <LI>else, if no method has side effects then the thread is idempotent;
     * <LI>else, if the first method has side effects and is complete then
     *     the thread is idempotent;
     * <LI>else, if the first method has side effects, is not complete,
     *     and no other method has side effects then the thread is idempotent;
     * <LI>else the thread is not idempotent.
     * </OL>
     *
     * <P>The major assumption here is that the side effects of any method
     * only apply to resource specified. E.g. a <tt>"PUT /barbara.html"</tt>
     * will only affect the resource "/barbara.html" and nothing else.
     * This assumption is violated by POST of course; however, POSTs are
     * not pipelined and will therefore never show up here.
     *
     * @param req the request
     */
    public boolean isIdempotent(Request req)
    {
	if (!analysis_done)
	    do_analysis();

	return ((Boolean) threads.get(req.getRequestURI())).booleanValue();
    }


    private static final Object INDET = new Object();

    private void do_analysis()
    {
	for (int idx=0; idx<r_len; idx++)
	{
	    Object t_state = threads.get(r_history[idx]);

	    if (m_history[idx] == UNKNOWN)
		threads.put(r_history[idx], Boolean.FALSE);
	    else if (t_state == null)		// new thread
	    {
		if (methodHasSideEffects(m_history[idx]))
		{
		    if (methodIsComplete(m_history[idx]))	// is idempotent
			threads.put(r_history[idx], Boolean.TRUE);
		    else
			threads.put(r_history[idx], Boolean.FALSE);
		}
		else					// indeterminate
		    threads.put(r_history[idx], INDET);
	    }
	    else				// update thread
	    {
		if (t_state == INDET  && methodHasSideEffects(m_history[idx]))
		    threads.put(r_history[idx], Boolean.FALSE);
	    }
	}

	// any thread still indeterminate must be idempotent
	Enumeration te = threads.keys();
	while (te.hasMoreElements())
	{
	    String res = (String) te.nextElement();
	    if (threads.get(res) == INDET)
		threads.put(res, Boolean.TRUE);
	}
    }


    /**
     * A method is idempotent if the side effects of N identical
     * requests is the same as for a single request (Section 9.1.2
     * of RFC-????).
     *
     * @return true if method is idempotent
     */
    public static boolean methodIsIdempotent(String method)
    {
	return methodIsIdempotent(methodNum(method));
    }


    private static boolean methodIsIdempotent(int method)
    {
	switch (method)
	{
	    case HEAD:
	    case GET:
	    case PUT:
	    case DELETE:
	    case OPTIONS:
	    case TRACE:
	    case PROPFIND:
	    case PROPPATCH:
	    case COPY:
	    case MOVE:
		return true;
	    case UNKNOWN:
	    case POST:
	    case MKCOL:
	    case LOCK:
	    case UNLOCK:
	    default:
		return false;
	}
    }


    /**
     * A method is complete if any side effects of the request affect
     * the complete resource. For example, a PUT is complete but a
     * PUT with byte-ranges wouldn't be. In essence, if a request uses
     * a method which has side effects and is complete then the state
     * of the resource after the request is independent of the state of
     * the resource before the request.
     *
     * @return true if method is complete
     */
    public static boolean methodIsComplete(String method)
    {
	return methodIsComplete(methodNum(method));
    }


    private static boolean methodIsComplete(int method)
    {
	switch (method)
	{
	    case HEAD:
	    case GET:
	    case PUT:
	    case DELETE:
	    case OPTIONS:
	    case TRACE:
	    case PROPFIND:
	    case COPY:
	    case MOVE:
	    case LOCK:
	    case UNLOCK:
		return true;
	    case UNKNOWN:
	    case POST:
	    case PROPPATCH:
	    case MKCOL:
	    default:
		return false;
	}
    }


    public static boolean methodHasSideEffects(String method)
    {
	return methodHasSideEffects(methodNum(method));
    }


    private static boolean methodHasSideEffects(int method)
    {
	switch (method)
	{
	    case HEAD:
	    case GET:
	    case OPTIONS:
	    case TRACE:
	    case PROPFIND:
	    case LOCK:
	    case UNLOCK:
		return false;
	    case UNKNOWN:
	    case POST:
	    case PUT:
	    case DELETE:
	    case PROPPATCH:
	    case MKCOL:
	    case COPY:
	    case MOVE:
	    default:
		return true;
	}
    }


    private static int methodNum(String method)
    {
	if (method.equals("GET"))
	    return GET;
	if (method.equals("POST"))
	    return POST;
	if (method.equals("HEAD"))
	    return HEAD;
	if (method.equals("PUT"))
	    return PUT;
	if (method.equals("DELETE"))
	    return DELETE;
	if (method.equals("OPTIONS"))
	    return OPTIONS;
	if (method.equals("TRACE"))
	    return TRACE;
	if (method.equals("PROPFIND"))
	    return PROPFIND;
	if (method.equals("PROPPATCH"))
	    return PROPPATCH;
	if (method.equals("MKCOL"))
	    return MKCOL;
	if (method.equals("COPY"))
	    return COPY;
	if (method.equals("MOVE"))
	    return MOVE;
	if (method.equals("LOCK"))
	    return LOCK;
	if (method.equals("UNLOCK"))
	    return UNLOCK;

	return UNKNOWN;
    }


    /**
     * Test code.
     */
    public static void main(String args[])
    {
	IdempotentSequence seq = new IdempotentSequence();

	seq.add(new Request(null, "GET", "/b1", null, null, null, false));
	seq.add(new Request(null, "PUT", "/b2", null, null, null, false));
	seq.add(new Request(null, "GET", "/b1", null, null, null, false));
	seq.add(new Request(null, "PUT", "/b3", null, null, null, false));
	seq.add(new Request(null, "GET", "/b2", null, null, null, false));
	seq.add(new Request(null, "POST", "/b8", null, null, null, false));
	seq.add(new Request(null, "PUT", "/b3", null, null, null, false));
	seq.add(new Request(null, "GET", "/b1", null, null, null, false));
	seq.add(new Request(null, "TRACE", "/b4", null, null, null, false));
	seq.add(new Request(null, "GET", "/b9", null, null, null, false));
	seq.add(new Request(null, "LINK", "/b4", null, null, null, false));
	seq.add(new Request(null, "GET", "/b4", null, null, null, false));
	seq.add(new Request(null, "PUT", "/b5", null, null, null, false));
	seq.add(new Request(null, "HEAD", "/b5", null, null, null, false));
	seq.add(new Request(null, "PUT", "/b5", null, null, null, false));
	seq.add(new Request(null, "POST", "/b9", null, null, null, false));
	seq.add(new Request(null, "GET", "/b6", null, null, null, false));
	seq.add(new Request(null, "DELETE", "/b6", null, null, null, false));
	seq.add(new Request(null, "HEAD", "/b6", null, null, null, false));
	seq.add(new Request(null, "OPTIONS", "/b7", null, null, null, false));
	seq.add(new Request(null, "TRACE", "/b7", null, null, null, false));
	seq.add(new Request(null, "GET", "/b7", null, null, null, false));
	seq.add(new Request(null, "PUT", "/b7", null, null, null, false));

	if (!seq.isIdempotent(new Request(null, null, "/b1", null, null, null, false)))
	    System.err.println("Sequence b1 failed");
	if (!seq.isIdempotent(new Request(null, null, "/b2", null, null, null, false)))
	    System.err.println("Sequence b2 failed");
	if (!seq.isIdempotent(new Request(null, null, "/b3", null, null, null, false)))
	    System.err.println("Sequence b3 failed");
	if (seq.isIdempotent(new Request(null, null, "/b4", null, null, null, false)))
	    System.err.println("Sequence b4 failed");
	if (!seq.isIdempotent(new Request(null, null, "/b5", null, null, null, false)))
	    System.err.println("Sequence b5 failed");
	if (seq.isIdempotent(new Request(null, null, "/b6", null, null, null, false)))
	    System.err.println("Sequence b6 failed");
	if (seq.isIdempotent(new Request(null, null, "/b7", null, null, null, false)))
	    System.err.println("Sequence b7 failed");
	if (seq.isIdempotent(new Request(null, null, "/b8", null, null, null, false)))
	    System.err.println("Sequence b8 failed");
	if (seq.isIdempotent(new Request(null, null, "/b9", null, null, null, false)))
	    System.err.println("Sequence b9 failed");

	System.out.println("Tests finished");
    }
}
