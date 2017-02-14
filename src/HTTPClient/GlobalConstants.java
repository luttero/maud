/*
 * @(#)GlobalConstants.java				0.3-3 06/05/2001
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


/**
 * This interface defines various global constants.
 *
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 * @since	V0.3
 */
interface GlobalConstants
{
    /** possible http protocols we (might) handle */
    int     HTTP       = 0; 	// plain http
    int     HTTPS      = 1; 	// http on top of SSL
    int     SHTTP      = 2; 	// secure http
    int     HTTP_NG    = 3; 	// http next-generation

    /** some known http versions */
    int     HTTP_1_0   = (1 << 16) + 0;
    int     HTTP_1_1   = (1 << 16) + 1;

    /** Content delimiters */
    int     CD_NONE    = 0; 	// raw read from the stream
    int     CD_HDRS    = 1; 	// reading headers/trailers
    int     CD_0       = 2; 	// no body
    int     CD_CLOSE   = 3; 	// by closing connection
    int     CD_CONTLEN = 4; 	// via the Content-Length header
    int     CD_CHUNKED = 5; 	// via chunked transfer encoding
    int     CD_MP_BR   = 6; 	// via multipart/byteranges
}
