/* jpvmDaemonMessageTag.java
 *
 * Message tags needed to communicate with the jpvmDaemons.
 *
 * Adam J Ferrari
 * Sun 05-26-1996
 *
 *
 * modified by
 * Luca Lutterotti
 * Verona, 12 November 1999
 *
 * Copyright (C) 1996  Adam J Ferrari, 1999 Luca Lutterotti
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
 * MA 02139, USA.
 */

package it.unitn.ing.rista.jpvm;

/**
 *
 * @version $Revision: 1.3 $, $Date: 2004/01/21 11:14:51 $
 * @author Adam J Ferrari
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public final class jpvmDaemonMessageTag {
  public static final int jpvmdPingRequest = 1;
  public static final int jpvmdPingReply = 2;
  public static final int jpvmdRegisterTask = 3;
  public static final int jpvmdSpawnTask = 4;
  public static final int jpvmdCreateTask = 5;
  public static final int jpvmdCreatedTask = 6;
  public static final int jpvmdDeleteTask = 7;
  public static final int jpvmdTaskStatus = 8;
  public static final int jpvmdAddHost = 9;
  public static final int jpvmdAddHostBcast = 10;
  public static final int jpvmdDeleteHost = 11;
  public static final int jpvmdDeleteHostBcast = 12;
  public static final int jpvmdHostStatus = 13;
  public static final int jpvmdHostHalt = 14;
  public static final int jpvmdHalt = 15;
  public static final int jpvmdRegisterChild = 16;
}
