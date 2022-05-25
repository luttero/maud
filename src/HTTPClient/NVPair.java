/*
 * @(#)NVPair.java					0.3-3 06/05/2001
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
 * This class holds a Name/Value pair of strings. It's used for headers,
 * form-data, attribute-lists, etc. This class is immutable.
 *
 * @version	0.3-3  06/05/2001
 * @author	Ronald Tschal\ufffdr
 */
public final class NVPair
{
    /** the name */
    private String name;

    /** the value */
    private String value;


    // Constructors

    /**
     * Creates a new name/value pair and initializes it to the
     * specified name and value.
     *
     * @param name  the name
     * @param value the value
     */
    public NVPair(String name, String value)
    {
	this.name  = name;
	this.value = value;
    }

    /**
     * Creates a copy of a given name/value pair.
     *
     * @param p the name/value pair to copy
     */
    public NVPair(NVPair p)
    {
	this(p.name, p.value);
    }


    // Methods

    /**
     * Get the name.
     *
     * @return the name
     */
    public final String getName()
    {
	return name;
    }

    /**
     * Get the value.
     *
     * @return the value
     */
    public final String getValue()
    {
	return value;
    }


    /**
     * Produces a string containing the name and value of this instance.
     *
     * @return a string containing the class name and the name and value
     */
    public String toString()
    {
	return getClass().getName() + "[name=" + name + ",value=" + value + "]";
    }
}
