/*
 * @(#)CODdatabaseConnector.java created Jul 13, 2005 Mesiano
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is 
 * provided as it is as confidential and proprietary information.  
 * You shall not disclose such Confidential Information and shall use 
 * it only in accordance with the terms of the license agreement you 
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.io.COD;

import it.unitn.ing.rista.util.Misc;

import java.sql.*;

/**
 * The CODdatabaseConnector is a class to connect to the COD database
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class CODdatabaseConnector {

  public void testConnection(String location, String user, String passwd) {
    // location something like:   "localhost/test"
    Statement stmt = null;
    ResultSet rs = null;

    try {
      Class.forName("com.mysql.jdbc.Driver").newInstance();

      Connection conn = DriverManager.getConnection("jdbc:mysql://" + location + "?user=" + user + "&password=" + passwd);

      // Do something with the Connection
      stmt = conn.createStatement();
      rs = stmt.executeQuery("SELECT * FROM data LIMIT 10");

      // or alternatively, if you don't know ahead of time that
      // the query will be a SELECT...

/*      if (stmt.execute("SELECT * FROM data LIMIT 10")) {
        rs = stmt.getResultSet();
      }*/


      // Now do something with the ResultSet ....
      while(rs.next()) {
      // Capitalization doesn't matter:
        System.out.println(
          rs.getString(1) + ", "
          + rs.getString(2)
          + ": " + rs.getString(3) );
      }
//      stmt.close(); // Also closes ResultSet
    } catch (SQLException ex) {
      // handle any errors
      ex.printStackTrace();
      System.out.println("SQLException: " + ex.getMessage());
      System.out.println("SQLState: " + ex.getSQLState());
      System.out.println("VendorError: " + ex.getErrorCode());
    } catch (Exception nf) {
      nf.printStackTrace();
    } finally {
      // it is a good idea to release
      // resources in a finally{} block
      // in reverse-order of their creation
      // if they are no-longer needed

      if (rs != null) {
        try {
          rs.close();
        } catch (SQLException sqlEx) { // ignore }

          rs = null;
        }

        if (stmt != null) {
          try {
            stmt.close();
          } catch (SQLException sqlEx) { // ignore }

            stmt = null;
          }
        }
      }
    }


  }


/*
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

// Notice, do not import com.mysql.jdbc.*
// or you will have problems!

public class LoadDriver {
    public static void main(String[] args) {
        try {
            // The newInstance() call is a work around for some
            // broken Java implementations

            Class.forName("com.mysql.jdbc.Driver").newInstance();
        } catch (Exception ex) {
            // handle the error
        }
}


import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

    ... try {
            Connection conn = DriverManager.getConnection("jdbc:mysql://localhost/test?user=monty&password=greatsqldb");

            // Do something with the Connection

           ....
        } catch (SQLException ex) {
            // handle any errors
            System.out.println("SQLException: " + ex.getMessage());
            System.out.println("SQLState: " + ex.getSQLState());
            System.out.println("VendorError: " + ex.getErrorCode());
        }


// assume conn is an already created JDBC connection
Statement stmt = null;
ResultSet rs = null;

try {
    stmt = conn.createStatement();
    rs = stmt.executeQuery("SELECT foo FROM bar");

    // or alternatively, if you don't know ahead of time that
    // the query will be a SELECT...

    if (stmt.execute("SELECT foo FROM bar")) {
        rs = stmt.getResultSet();
    }

    // Now do something with the ResultSet ....
} finally {
    // it is a good idea to release
    // resources in a finally{} block
    // in reverse-order of their creation
    // if they are no-longer needed

    if (rs != null) {
        try {
            rs.close();
        } catch (SQLException sqlEx) { // ignore }

        rs = null;
    }

    if (stmt != null) {
        try {
            stmt.close();
        } catch (SQLException sqlEx) { // ignore }

        stmt = null;
    }
}

*/
}
