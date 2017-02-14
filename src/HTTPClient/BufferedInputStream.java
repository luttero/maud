/*
 * @(#)BufferedInputStream.java				0.3-3 06/05/2001
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

import java.io.InputStream;
import java.io.FilterInputStream;
import java.io.IOException;

/**
 * This class is similar to java.io.BufferedInputStream, except that it fixes
 * certain bugs and provides support for finding multipart boundaries.
 *
 * <P>Note: none of the methods here are synchronized because we assume the
 * caller is already taking care of that.
 *
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 */
class BufferedInputStream extends FilterInputStream
{
    /** our read buffer */
    private byte[] buffer = new byte[2000];
    /** the next byte in the buffer at which to read */
    private int    pos = 0;
    /** the end of the valid data in the buffer */
    private int    end = 0;
    /** the current mark position, or -1 if none */
    private int    mark_pos = -1;
    /**
     * the large read threashhold: reads larger than this aren't buffered if
     * both the current buffer is empty and no mark has been set. This is just
     * an attempt to balance copying vs. multiple reads.
     */
    private int    lr_thrshld = 1500;


    /**
     * Create a new BufferedInputStream around the given input stream.
     *
     * @param stream  the underlying input stream to use
     */
    BufferedInputStream(InputStream stream)
    {
	super(stream);
    }

    /**
     * Read a single byte.
     *
     * @return the read byte, or -1 if the end of the stream has been reached
     * @exception IOException if thrown by the underlying stream
     */
    public int read() throws IOException
    {
	if (pos >= end)
	    fillBuff();

	return (end > pos) ? (buffer[pos++] & 0xFF) : -1;
    }

    /**
     * Read a buffer full.
     *
     * @param buf  the buffer to read into
     * @param off  the offset within <var>buf</var> at which to start writing
     * @param len  the number of bytes to read
     * @return the number of bytes read
     * @exception IOException if thrown by the underlying stream
     */
    public int read(byte[] buf, int off, int len) throws IOException
    {
	if (len <= 0)
	    return 0;

	// optimize for large reads
	if (pos >= end  &&  len >= lr_thrshld  &&  mark_pos < 0)
	    return in.read(buf, off, len);

	if (pos >= end)
	    fillBuff();

	if (pos >= end)
	    return -1;

	int left = end - pos;
	if (len > left)
	    len = left;
	System.arraycopy(buffer, pos, buf, off, len);
	pos += len;

	return len;
    }

    /**
     * Skip the given number of bytes in the stream.
     *
     * @param n   the number of bytes to skip
     * @return the actual number of bytes skipped
     * @exception IOException if thrown by the underlying stream
     */
    public long skip(long n) throws IOException
    {
	if (n <= 0)
	    return 0;

	int left = end - pos;
	if (n <= left)
	{
	    pos += n;
	    return n;
	}
	else
	{
	    pos = end;
	    return left + in.skip(n - left);
	}
    }

    /**
     * Fill buffer by reading from the underlying stream. This assumes the
     * current buffer is empty, i.e. pos == end.
     */
    private final void fillBuff() throws IOException
    {
	if (mark_pos > 0)	// keep the marked stuff around if possible
	{
	    // only copy if we don't have any space left
	    if (end >= buffer.length)
	    {
		System.arraycopy(buffer, mark_pos, buffer, 0, end - mark_pos);
		pos = end - mark_pos;
	    }
	}
	else if (mark_pos == 0  &&  end < buffer.length)
	    ;			// pos == end, so we just fill what's left
	else
	    pos = 0;		// try to fill complete buffer

	// make sure our state is consistent even if read() throws InterruptedIOException
	end = pos;

	int got = in.read(buffer, pos, buffer.length - pos);
	if (got > 0)
	    end = pos + got;
    }

    /**
     * @return the number of bytes available for reading without blocking
     * @exception IOException if the buffer is empty and the underlying stream has been
     *                        closed
     */
    public int available() throws IOException
    {
	int avail = end - pos;  
	if (avail == 0)
	    return in.available();

	try
	    { avail += in.available(); }
	catch (IOException ignored)
	    { /* ignore this because we have something available */ }
	return avail;
    }

    /**
     * Mark the current read position so that we can start searching for the end boundary.
     */
    void markForSearch()
    {
	mark_pos = pos;
    }

    /**
     * Figures out how many bytes past the end of the multipart we read. If we
     * found the end, it then resets the read pos to just past the end of the
     * boundary and unsets the mark; if not found, is sets the mark_pos back
     * enough from the current position so we can always be sure to find the
     * boundary.
     *
     * @param search     the search string (end boundary)
     * @param search_cmp the compiled info of the search string
     * @return how many bytes past the end of the boundary we went; -1 if we
     *         haven't gone passed it yet.
     */
    int pastEnd(byte[] search, int[] search_cmp)
    {
	int idx = Util.findStr(search, search_cmp, buffer, mark_pos, pos);
	if (idx == -1)
	    mark_pos = (pos > search.length) ? pos - search.length : 0;
	else
	{
	    int eos  = idx + search.length;
	    idx      = pos - eos;
	    pos      = eos;
	    mark_pos = -1;
	}

	return idx;
    }
}
