/*
 * KappaLayout, a Java layout manager.
 * Copyright (C) 2000, Dale Anson
 * modified by L. Lutterotti
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package it.unitn.ing.rista.awt;

import java.awt.LayoutManager2;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Insets;
import java.util.BitSet;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * KappaLayout -- similar to others, but this one's simpler and easier to use.
 * Example use:
 * This will put a button on a panel in the top of its cell, stretched to
 * fill the cell width, with a 3 pixel pad:
 * <p>
 * <code>
 * Panel p = new Panel(new KappaLayout());
 * Button b = new Button("OK");
 * p.add(b, "0, 0, 1, 2, 2, w, 3");
 * </code>
 * <p>
 * The constraints string has this layout:
 * "x, y, w, h, a, s, p"
 * defined as follows:
 * <ul>
 * <li>'x' is the column to put the component, default is 0
 * <li>'y' is the row to put the component, default is 0
 * <li>'w' is the width of the component in columns (column span), default is 1
 * <li>'h' is the height of the component in rows (row span), default is 1
 * <li>'a' is the alignment within the cell; 'a' can be a value between 0 and 8,
 * inclusive, (default is 0) and causes the alignment of the component within the cell to follow
 * this pattern:
 * 8 1 2
 * 7 0 3
 * 6 5 4, or
 * 0 horizontal center, vertical center,
 * 1 horizontal center, vertical top,
 * 2 horizontal right, vertical top,
 * 3 horizontal right, vertical center,
 * 4 horizontal right, vertical bottom,
 * 5 horizontal center, vertical bottom,
 * 6 horizontal left, vertical bottom,
 * 7 horizontal left, vertical center,
 * 8 horizontal left, vertical top.
 * <li>'s' is the stretch value. 's' can have these values:
 * 'w' stretch to fill cell width
 * 'h' stretch to fill cell height
 * 'wh' or 'hw' stretch to fill both cell width and cell height
 * '0' (character 'zero') no stretch (default)
 * <li>'p' is the amount of padding to put around the component. This much blank
 * space will be applied on all sides of the component, default is 0.
 * </ul>
 * Parameters may be omitted (default  values will be used), e.g.,
 * <code> p.add(new Button("OK), "1,4,,,w,");</code>
 * which means put the button at column 1, row 4, default 1 column wide, default
 * 1 row tall, stretch to fit width of column, no padding.
 * Spaces in the parameter string are ignored, so these are identical:<br>
 * <code> p.add(new Button("OK), "1,4,,,w,");</code>
 * <code> p.add(new Button("OK), " 1, 4,   , , w");</code><p>
 * Rather than use a constraints string, a Constraints object may be used
 * directly, similar to how GridBag uses a GridBagConstraint. E.g,
 * <code>
 * Panel p = new Panel();
 * KappaLayout tl = new KappaLayout();
 * p.setLayout(tl);<br>
 * KappaLayout.Constraints con = tl.getConstraint();
 * con.x = 1;
 * con.y = 2;
 * con.w = 2;
 * con.h = 2;
 * con.s = "wh";
 * panel.add(new Button("OK"), con);
 * con.x = 3;
 * panel.add(new Button("Cancel"), con);
 * </code>
 * Note that the same Constraints can be reused, thereby reducing the number of
 * objects created.<p>
 * @author Dale Anson
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/07/20 13:39:02 $
 */

public class KappaLayout implements LayoutManager2 {
  // overall preferred width of components in container
  private int _preferred_width = 0;

  // overall preferred height of components in container
  private int _preferred_height = 0;

  // if true, then _preferred_width and _preferred_size are unknown
  private boolean _size_unknown = true;

  private int _col_count = 0;               // number of columns in the layout
  private int _row_count = 0;               // number of rows in the layout
  private boolean _cols_unknown = true;     // if true, then _col_count is unknown or invalid
  private boolean _rows_unknown = true;     // if true, then _row_count is unknown or invalid

  // storage for component constraints
  // key is the component, value is a Constraints
  private Hashtable _constraints = new Hashtable();

  // model of the table, one Dimension per cell in the table, the Dimension
  // holds the width and height of the component in the cell
  private Dimension[][] _table = null;

  // model of the component layout in the table
  private Component[][] _components = null;

  // in both _col_widths and _row_heights, if the number is negative, the column or
  // row will be treated as having a fixed width or height. During layout, the negative
  // numbers will be treated as positive numbers -- negative width and negative height
  // being meaningless concepts.
  private int[] _col_widths;    // stores a width per column which is the widest preferred width of components in each column
  private int[] _row_heights;   // stores a height per row which is the tallest preferred height of components in each row

  // storage for columns and rows that want to be the same size. Each element
  // in these vectors is an int[], with each item in the array being a column
  // or row number.
  private Vector _same_width_cols;
  private Vector _same_height_rows;

  // if true, spread extra space equally between components in both directions,
  // doesn't stretch the components, just the space between them
  private boolean _stretch = false;


  /**
   * Default constructor, no stretching.
   */
  public KappaLayout() {
    this(false);
  }

  /**
   * Constructor, allows stretching.
   * @param s if true, stretches layout to fill container by adding extra space
   * between components, does not stretch the components, just the space between them.
   */
  public KappaLayout(boolean s) {
    _stretch = s;
  }

  /**
   * Useful for debugging a layout. Call after components are showing to make
   * sure all data is available.
   * @return a String with lots of useful info about the layout.
   */
  public String toString() {
    StringBuffer sb = new StringBuffer();
    sb.append("-------------------------------\n");
    sb.append(getClass().getName() + ":\n");
    sb.append("columns=" + _col_count);
    sb.append(", rows=" + _row_count + "\n");
    sb.append("preferred width=" + _preferred_width);
    sb.append(", preferred height=" + _preferred_height + "\n");
    if (_col_widths != null) {
      sb.append("column widths (left to right):");
      for (int i = 0; i < _col_widths.length; i++)
        sb.append(_col_widths[i] + ",");
    }
    if (_row_heights != null) {
      sb.append("\nrow heights (top to bottom):");
      for (int i = 0; i < _row_heights.length; i++)
        sb.append(_row_heights[i] + ",");
    }
    if (_constraints != null) {
      sb.append("\ncomponents (no order):\n");
      Enumeration enum1 = _constraints.keys();
      while (enum1.hasMoreElements())
        sb.append(enum1.nextElement() + "\n");
    }
    sb.append("-------------------------------\n");
    return sb.toString();
  }

  /**
   * Required by LayoutManager, simply calls <code>addLayoutComponent(Component, Object)</code>.
   */
  public void addLayoutComponent(String n, Component c) {
    addLayoutComponent(c, n);
  }

  /**
   * Required by LayoutManager.
   */
  public void removeLayoutComponent(Component c) {
    _constraints.remove(c);
    _size_unknown = true;
    _cols_unknown = true;
    _rows_unknown = true;
  }

  /**
   * Required by LayoutManager.
   */
  public Dimension preferredLayoutSize(Container parent) {
    Dimension dim = new Dimension(0, 0);
    if (_size_unknown)
      calculateDimensions();
    Insets insets = parent.getInsets();
    dim.width = _preferred_width + insets.left + insets.right;
    dim.height = _preferred_height + insets.top + insets.bottom;
    return dim;
  }

  /**
   * Required by LayoutManager.
   * @return <code>preferredLayoutSize(parent)</code>
   */
  public Dimension minimumLayoutSize(Container parent) {
    return preferredLayoutSize(parent);
  }

  /**
   * Required by LayoutManager, does all the real layout work.
   */
  public void layoutContainer(Container parent) {
    Insets insets = parent.getInsets();
    int max_width = parent.getSize().width - (insets.left + insets.right);
    int max_height = parent.getSize().height - (insets.top + insets.bottom);
    int x = insets.left;    // x and y location to put component
    int y = insets.top;
    int xfill = 0;          // how much extra space to put between components
    int yfill = 0;          // when stretching to fill entire container

    // make sure preferred size is known, a side effect is that countColumns
    // and countRows are automatically called.
    if (_size_unknown) {
      calculateDimensions();
    }

    // if necessary, calculate the amount of padding to add between the
    // components to fill the container
    if (_stretch) {
      if (max_width > _preferred_width && _col_count > 1) {
        xfill = (max_width - _preferred_width) / (_col_count - 1);
      }
      if (max_height > _preferred_height && _row_count > 1) {
        yfill = (max_height - _preferred_height) / (_row_count - 1);
      }
    }

    // do the layout. Components are handled by columns, top to bottom,
    // left to right.
    for (int i = 0; i < _col_count; i++) {
      y = insets.top;
      if (i > 0) {
        x += Math.abs(_col_widths[i - 1]);
        if (_stretch && i < _col_count) {
          x += xfill;
        }
      }

      for (int j = 0; j < _row_count; j++) {
        if (j > 0) {
          y += Math.abs(_row_heights[j - 1]);
          if (_stretch && j < _row_count) {
            y += yfill;
          }
        }
        Component c = _components[i][j];
        if (c != null && c.isVisible()) {
          Dimension d = c.getPreferredSize();
          Constraints q = (Constraints) _constraints.get(c);

          // calculate width of spanned columns
          int sum_cols = 0;
          for (int n = i; n < i + q.w; n++) {
            sum_cols += Math.abs(_col_widths[n]);
          }

          // calculate width of spanned rows
          int sum_rows = 0;
          for (int n = j; n < j + q.h; n++) {
            sum_rows += Math.abs(_row_heights[n]);
          }

          // stretch and pad if required
          if (q.s.indexOf("w") != -1) {
            d.width = sum_cols - q.p * 2;
          }
          if (q.s.indexOf("h") != -1) {
            d.height = sum_rows - q.p * 2;
          }

          // calculate adjustment
          int x_adj = sum_cols - d.width;  // max amount to put component at right edge of spanned cell(s)
          int y_adj = sum_rows - d.height; // max amount to put component at bottom edge of spanned cell(s)

          // in each case, add the correction for the cell, then subtract
          // the correction after applying it.  This prevents the corrections
          // from improperly accumulating across cells. Padding must be handled
          // explicitly for each case.
          // Alignment follows this pattern within the spanned cells:
          // 8 1 2
          // 7 0 3
          // 6 5 4
          switch (q.a) {
            case 1:     // top center
              x += x_adj / 2;
              y += q.p;
              c.setBounds(x, y, d.width, d.height);
              x -= x_adj / 2;
              y -= q.p;
              break;
            case 2:     // top right
              x += x_adj - q.p;
              y += q.p;
              c.setBounds(x, y, d.width, d.height);
              x -= x_adj - q.p;
              y -= q.p;
              break;
            case 3:     // center right
              x += x_adj - q.p;
              y += y_adj / 2;
              c.setBounds(x, y, d.width, d.height);
              x -= x_adj - q.p;
              y -= y_adj / 2;
              break;
            case 4:     // bottom right
              x += x_adj - q.p;
              y += y_adj - q.p;
              c.setBounds(x, y, d.width, d.height);
              x -= x_adj - q.p;
              y -= y_adj - q.p;
              break;
            case 5:     // bottom center
              x += x_adj / 2;
              y += y_adj - q.p;
              c.setBounds(x, y, d.width, d.height);
              x -= x_adj / 2;
              y -= y_adj - q.p;
              break;
            case 6:     // bottom left
              x += q.p;
              y += y_adj - q.p;
              c.setBounds(x, y, d.width, d.height);
              x -= q.p;
              y -= y_adj - q.p;
              break;
            case 7:     // center left
              x += q.p;
              y += y_adj / 2;
              c.setBounds(x, y, d.width, d.height);
              x -= q.p;
              y -= y_adj / 2;
              break;
            case 8:     // top left
              x += q.p;
              y += q.p;
              c.setBounds(x, y, d.width, d.height);
              x -= q.p;
              y -= q.p;
              break;
            case 0:     // dead center
            default:
              x += x_adj / 2;
              y += y_adj / 2;
              c.setBounds(x, y, d.width, d.height);
              x -= x_adj / 2;
              y -= y_adj / 2;
              break;
          }
        }
      }
    }
  }

  /**
   * Required by LayoutManager2. Does nothing if either component or constraints are null.
   */
  public void addLayoutComponent(Component comp, Object constraint) {
    // a couple of cases where there might be an early return...
    if (comp == null || constraint == null) {
      return;
    }

    if (constraint instanceof Constraints) {
      _constraints.put(comp, ((Constraints) constraint).clone());
      return;
    }

    // parse constraint into tokens
    Vector tokens = new Vector();
    String token;
    if (constraint instanceof String) {
      String c = constraint.toString();
      while (c.length() > 0) {
        int comma = c.indexOf(',');
        if (comma != -1) {
          token = c.substring(0, comma);
          c = c.substring(comma + 1);
        } else {
          token = c;
          c = "";
        }
        token = token.trim();
        if (token != null && token.length() > 0)
          tokens.addElement(token);
        else
          tokens.addElement("-1");
      }
    }

    // turn tokens into a Constraints...
    // get column
    Constraints q = new Constraints();
    Enumeration enum1 = tokens.elements();
    if (enum1.hasMoreElements()) {
      token = enum1.nextElement().toString();
      try {
        q.x = Integer.parseInt(token);
        if (q.x < 0)
          q.x = 0;
      } catch (Exception e) {
        q.x = 0;
      }
    }

    // get row
    if (enum1.hasMoreElements()) {
      token = enum1.nextElement().toString();
      try {
        q.y = Integer.parseInt(token);
        if (q.y < 0)
          q.y = 0;
      } catch (Exception e) {
        q.y = 0;
      }
    }

    // get column span
    if (enum1.hasMoreElements()) {
      token = enum1.nextElement().toString();
      try {
        q.w = Integer.parseInt(token);
        if (q.w < 1)
          q.w = 1;
      } catch (Exception e) {
        q.w = 1;
      }
    }

    // get row span
    if (enum1.hasMoreElements()) {
      token = enum1.nextElement().toString();
      try {
        q.h = Integer.parseInt(token);
        if (q.h < 1)
          q.h = 1;
      } catch (Exception e) {
        q.h = 1;
      }
    }

    // get alignment
    if (enum1.hasMoreElements()) {
      token = enum1.nextElement().toString();
      try {
        q.a = Integer.parseInt(token);
        if (q.a < 0)
          q.a = 0;
      } catch (Exception e) {
        q.a = 0;
      }
    }

    // get component stretch
    if (enum1.hasMoreElements()) {
      token = enum1.nextElement().toString().trim().toLowerCase();
      if (token.equals("w") || token.equals("h") || token.equals("wh") || token.equals("hw"))
        q.s = token;
      else
        q.s = "0";
    }

    // get component padding
    if (enum1.hasMoreElements()) {
      token = enum1.nextElement().toString();
      try {
        q.p = Integer.parseInt(token);
        if (q.p < 0)
          q.p = 0;
      } catch (Exception e) {
        q.p = 0;
      }
    }

    // save the component and its constraints for later use
    _constraints.put(comp, q);

    // indicate table layout is now invalid
    _size_unknown = true;
    _cols_unknown = true;
    _rows_unknown = true;
  }

  /**
   * Required by LayoutManager2.
   * @return <code>preferredLayoutSize(parent)</code>
   */
  public Dimension maximumLayoutSize(Container c) {
    return preferredLayoutSize(c);
  }

  /**
   * Required by LayoutManager2.
   */
  public float getLayoutAlignmentX(Container c) {
    return 0.5f;   // beats me what this is for, so I'm defaulting to centered.
  }

  /**
   * Required by LayoutManager2.
   */
  public float getLayoutAlignmentY(Container c) {
    return 0.5f;   // beats me what this is for, so I'm defaulting to centered.
  }

  /**
   * Required by LayoutManager2.
   */
  public void invalidateLayout(Container c) {
  }

  /**
   * Calculate preferred size and other dimensions.
   */
  private void calculateDimensions() {
    _preferred_width = 0;
    _preferred_height = 0;
    Dimension dim = null;

    // confirm column and row counts have been calculated
    countColumns();
    countRows();

    // set up two arrays the same size as the table, these are used to help
    // calculate preferred size
    _table = new Dimension[_col_count][_row_count];
    _components = new Component[_col_count][_row_count];

    // fill with default values
    for (int i = 0; i < _col_count; i++) {
      for (int j = 0; j < _row_count; j++) {
        _table[i][j] = new Dimension(0, 0);
        _components[i][j] = null;
      }
    }

    // set up storage for max col width and max row height.  These arrays
    // have an entry per column and row and will hold the largest width or
    // height of components in each column or row respectively.
    int[] temp;
    if (_col_widths != null) {
      // if column count has changed, need to preserve existing column widths
      // in case one or more remaining columns has been set to a fixed width.
      temp = new int[_col_widths.length];
      System.arraycopy(_col_widths, 0, temp, 0, _col_widths.length);
      _col_widths = new int[_col_count];
      System.arraycopy(temp, 0, _col_widths, 0, Math.min(temp.length, _col_widths.length));
    } else {
      _col_widths = new int[_col_count];
    }
    if (_row_heights != null) {
      // if row count has changed, need to preserve existing row heights
      // in case one or more remaining rows has been set to a fixed height.
      temp = new int[_row_heights.length];
      System.arraycopy(_row_heights, 0, temp, 0, _row_heights.length);
      _row_heights = new int[_row_count];
      System.arraycopy(temp, 0, _row_heights, 0, Math.min(temp.length, _row_heights.length));
    } else {
      _row_heights = new int[_row_count];
    }

    // get the constraints
    Enumeration enum1 = _constraints.keys();
    while (enum1.hasMoreElements()) {
      Component c = (Component) enum1.nextElement();
      Constraints q = (Constraints) _constraints.get(c);

      // store the component in its (x, y) location
      _components[q.x][q.y] = c;

      // as components can span columns and rows, store the maximum dimension
      // of the component that could be in the spanned cells. Note that it
      // may happen that none of the component is actually in the cell.
      dim = c.getPreferredSize();
      dim.width += q.p * 2;   // adjust for padding if necessary
      dim.height += q.p * 2;
      dim.width /= q.w;
      dim.height /= q.h;
      for (int i = q.x; i < q.x + q.w; i++) {
        for (int j = q.y; j < q.y + q.h; j++) {
          _table[i][j] = dim;
        }
      }
    }

    // calculate preferred width
    int col_width = 0;
    for (int i = 0; i < _col_count; i++) {
      for (int j = 0; j < _row_count; j++) {
        Dimension p = _table[i][j];
        col_width = Math.max(p.width, col_width);
      }

      // store max width of each column
      if (_col_widths[i] >= 0) {
        _col_widths[i] = col_width;
        _preferred_width += col_width;
      } else {
        _preferred_width += Math.abs(_col_widths[i]);
      }
      col_width = 0;
    }

    // adjust for same width columns
    if (_same_width_cols != null) {
      enum1 = _same_width_cols.elements();
      while (enum1.hasMoreElements()) {
        int[] same = (int[]) enum1.nextElement();
        // find widest column of this group
        int widest = same[0];
        for (int i = 0; i < same.length; i++) {
          if (same[i] < _col_widths.length)
            widest = Math.max(widest, _col_widths[same[i]]);
        }
        // now set all columns to this widest width
        for (int i = 0; i < same.length; i++) {
          if (same[i] < _col_widths.length) {
            _preferred_width += widest - _col_widths[same[i]];
            _col_widths[same[i]] = widest;
          }
        }
      }
    }

    // calculate preferred height
    int row_height = 0;
    for (int j = 0; j < _row_count; j++) {
      for (int i = 0; i < _col_count; i++) {
        Dimension p = _table[i][j];
        row_height = Math.max(p.height, row_height);
      }

      // store max height of each row
      if (_row_heights[j] >= 0) {
        _row_heights[j] = row_height;
        _preferred_height += row_height;
      } else {
        _preferred_height += Math.abs(_row_heights[j]);
      }
      row_height = 0;
    }

    // adjust for same height rows
    if (_same_height_rows != null) {
      enum1 = _same_height_rows.elements();
      while (enum1.hasMoreElements()) {
        int[] same = (int[]) enum1.nextElement();
        // find tallest row of this group
        int tallest = same[0];
        for (int i = 0; i < same.length; i++) {
          if (same[i] < _row_heights.length)
            tallest = Math.max(tallest, _row_heights[same[i]]);
        }
        // now set all rows to this tallest height
        for (int i = 0; i < same.length; i++) {
          if (same[i] < _row_heights.length) {
            _preferred_height += tallest - _row_heights[same[i]];
            _row_heights[same[i]] = tallest;
          }
        }
      }
    }

    _size_unknown = false;
  }

  /**
   * Calculate number of columns in table.
   */
  private void countColumns() {
    // early return...
    if (!_cols_unknown)
      return;

    Hashtable rows = new Hashtable();
//    Hashtable col_widths = new Hashtable();
//    int row_count = 0;

    // get the constraints
    Enumeration enum1 = _constraints.elements();
    while (enum1.hasMoreElements()) {
      Constraints q = (Constraints) enum1.nextElement();

      // figure out which columns this component spans
      BitSet row = null;
      String Y = String.valueOf(q.y);
      if (!rows.containsKey(Y)) {
        row = new BitSet();
        rows.put(Y, row);
      }
      row = (BitSet) rows.get(Y);
      for (int i = q.x; i < q.x + q.w; i++)
        row.set(i);
    }

    // calculate the number of columns
    enum1 = rows.elements();
    while (enum1.hasMoreElements()) {
      BitSet row = (BitSet) enum1.nextElement();
      for (int i = 0; i < row.size(); i++) {
        if (row.get(i))
          _col_count = Math.max(_col_count, i);
      }
    }
    _col_count += 1;
    _cols_unknown = false;
  }

  /**
   * Calculate number of rows in table.
   */
  private void countRows() {
    // early return...
    if (!_rows_unknown)
      return;

    Hashtable cols = new Hashtable();
//    int col_count = 0;
    Enumeration enum1 = _constraints.elements();
    while (enum1.hasMoreElements()) {
      Constraints q = (Constraints) enum1.nextElement();
      BitSet col = null;
      String X = String.valueOf(q.x);
      if (!cols.containsKey(X)) {
        col = new BitSet();
        cols.put(X, col);
      }
      col = (BitSet) cols.get(X);
      for (int i = q.y; i < q.y + q.h; i++)
        col.set(i);
    }
    enum1 = cols.elements();
    while (enum1.hasMoreElements()) {
      BitSet col = (BitSet) enum1.nextElement();
      for (int i = 0; i < col.size(); i++) {
        if (col.get(i))
          _row_count = Math.max(_row_count, i);
      }
    }
    _row_count += 1;
    _rows_unknown = false;
  }

  /**
   * Makes two columns be the same width.  The actual width will be the larger
   * of the preferred widths of these columns.
   * @param column1 column number
   * @param column2 column number
   */
  public void makeColumnsSameWidth(int column1, int column2) {
    int[] same = new int[2];
    same[0] = column1;
    same[1] = column2;
    makeColumnsSameWidth(same);
  }

  /**
   * Makes several columns be the same width.  The actual width will be the largest
   * preferred width of these columns.
   * @param columns array of column numbers to make the same width.
   */
  public void makeColumnsSameWidth(int[] columns) {
    if (columns.length <= 1)
      return;
    for (int i = 0; i < columns.length; i++) {
      if (columns[i] < 0)
        throw new IllegalArgumentException("Column parameter must be greater than 0.");
    }
    if (_same_width_cols == null)
      _same_width_cols = new Vector();
    _same_width_cols.addElement(columns);
  }

  /**
   * Makes two rows be the same height.  The actual height will be the larger
   * of the preferred heights of these rows.
   * @param row1 row number
   * @param row2 row number
   */
  public void makeRowsSameHeight(int row1, int row2) {
    int[] same = new int[2];
    same[0] = row1;
    same[1] = row2;
    makeRowsSameHeight(same);
  }

  /**
   * Makes several rows be the same height.  The actual height will be the largest
   * preferred height of these rows.
   * @param rows  array of row numbers to make the same height.
   */
  public void makeRowsSameHeight(int[] rows) {
    if (rows.length <= 1)
      return;
    for (int i = 0; i < rows.length; i++) {
      if (rows[i] < 0)
        throw new IllegalArgumentException("Row parameter must be greater than 0.");
    }
    if (_same_height_rows == null)
      _same_height_rows = new Vector();
    _same_height_rows.addElement(rows);
  }

  /**
   * Sets a column to a specific width.  Use care with this method, components
   * wider than the set width will be truncated.
   * @param column column number
   * @param width width in pixels
   */
  public void setColumnWidth(int column, int width) {
    if (_col_widths == null)
      _col_widths = new int[width];
    if (column < 0)
      throw new IllegalArgumentException("Column must be >= 0.");
    if (column > _col_widths.length - 1)
      throw new IllegalArgumentException("Column parameter too large.");
    // store fixed width columns as a negative number
    _col_widths[column] = -1 * width;
    _size_unknown = true;
  }

  /**
   * Sets a row to a specific height.  Use care with this method, components
   * taller than the set height will be truncated.
   * @param row row number
   * @param height height in pixels
   */
  public void setRowHeight(int row, int height) {
    if (_row_heights == null)
      _row_heights = new int[height];
    if (row < 0)
      throw new IllegalArgumentException("Row must be >= 0.");
    if (row > _row_heights.length - 1)
      throw new IllegalArgumentException("Row parameter too large.");
    // store fixed height rows as a negative number
    _row_heights[row] = -1 * height;
    _size_unknown = true;
  }

  /**
   * Creates a Constraints for direct manipulation.
   * @return a Constraints object for direct manipulation.
   */
  public static Constraints createConstraint() {
    return new Constraints();
  }

  /**
   * Useful for holding an otherwise empty column to a specific width.
   * @param width desired width of component
   * @return a component with some width but no height
   */
  public static Component createHorizontalStrut(int width) {
    return new Strut(width, 0);
  }

  /**
   * Useful for holding an otherwise empty row to a specific height.
   * @param height desired height of component
   * @return a component with some height but no width
   */
  public static Component createVerticalStrut(int height) {
    return new Strut(0, height);
  }

  /**
   * Useful for setting an otherwise blank cell to a specific width and height.
   * @param width desired width of component
   * @param height desired height of component
   * @return a component with some height and width, treated specially by KappaLayout
   * in that if a strut is in a column or row, that column or row will be restricted
   * to the length of the strut.
   */
  public static Component createStrut(int width, int height) {
    return new Strut(width, height);
  }

  /**
   * Simple component that is invisible.
   */
  public static class Strut extends Component {
    private Dimension dim;

    /**
     * @param w width
     * @param h height
     */
    public Strut(int w, int h) {
      dim = new Dimension(w, h);
    }

    /**
     * Overrides <code>getPreferredSize</code> from Component.
     */
    public Dimension getPreferredSize() {
      return dim;
    }
  }


  /**
   * This class is cloneable so that users may create and use a Constraints object
   * similar to how one would use a GridBagConstraints rather than the string parameters.
   */
  public static class Constraints extends Object implements Cloneable {
    /**
     * start column
     */
    public int x = 0;

    /**
     * start row
     */
    public int y = 0;

    /**
     * # columns wide
     */
    public int w = 1;

    /**
     * # rows high
     */
    public int h = 1;

    /**
     * alignment within cell, see comments in KappaLayout
     */
    public int a = 0;

    /**
     * stretch: default is 0 (no stretch), w = width of cell, h = height of cell, wh = both width and height
     */
    public String s = "0";

    /**
     * padding, same amount of blank space on all four sides of component
     */
    public int p = 0;

    /**
     * @return a clone of this object.
     */
    public Object clone() {
      try {
        return super.clone();
      } catch (Exception e) {
        return null;
      }
    }
  }
}

