/*
 * @(#)DownFlowLayout.java created 31/01/2001 Mesiano
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.awt;

import java.awt.*;

/**
 * A down flow layout arranges components in a up-to-down flow, much
 * like lines of text in a paragraph. Down flow layouts are typically used
 * to arrange buttons in a panel. It will arrange
 * buttons up to down until no more buttons fit on the same column.
 * Each component is centered.
 * <p>
 * A down flow layout lets each component assume its natural (preferred) size.
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class DownFlowLayout implements LayoutManager, java.io.Serializable {
  /**
   * This value indicates that each row of components
   * should be top-justified.
   * @since   JDK1.1
   */
  public static final int TOP = 0;

  /**
   * This value indicates that each row of components
   * should be centered.
   * @since   JDK1.1
   */
  public static final int CENTER = 1;

  /**
   * This value indicates that each row of components
   * should be bottom-justified.
   * @since   JDK1.1
   */
  public static final int BOTTOM = 2;

  int align;
  int hgap;
  int vgap;

  /*
   * JDK 1.1 serialVersionUID
   */
  private static final long serialVersionUID = -7272534875583282632L;

  /**
   * Constructs a new Down Flow Layout with a centered alignment and a
   * default 5-unit horizontal and vertical gap.
   * @since JDK1.1
   */
  public DownFlowLayout() {
    this(CENTER, 5, 5);
  }

  /**
   * Constructs a new Down Flow Layout with the specified alignment and a
   * default 5-unit horizontal and vertical gap.
   * The value of the alignment argument must be one of
   * <code>DownFlowLayout.TOP</code>, <code>DownFlowLayout.BOTTOM</code>,
   * or <code>DownFlowLayout.CENTER</code>.
   * @param align the alignment value
   * @since JDK1.1
   */
  public DownFlowLayout(int align) {
    this(align, 5, 5);
  }

  /**
   * Creates a new down flow layout manager with the indicated alignment
   * and the indicated horizontal and vertical gaps.
   *
   * <p>The value of the alignment argument must be one of
   * <code>DownFlowLayout.TOP</code>, <code>DownFlowLayout.BOTTOM</code>,
   * or <code>DownFlowLayout.CENTER</code>.
   * @param      align   the alignment value.
   * @param      hgap    the horizontal gap between components.
   * @param      vgap    the vertical gap between components.
   * @since      JDK1.1
   */
  public DownFlowLayout(int align, int hgap, int vgap) {
    this.align = align;
    this.hgap = hgap;
    this.vgap = vgap;
  }

  /**
   * Gets the alignment for this layout.
   * Possible values are <code>DownFlowLayout.TOP</code>,
   * <code>DownFlowLayout.BOTTOM</code>, or <code>DownFlowLayout.CENTER</code>.
   * @return     the alignment value for this layout.
   * @see        DownFlowLayout#setAlignment
   * @since      JDK1.1
   */
  public int getAlignment() {
    return align;
  }

  /**
   * Sets the alignment for this layout.
   * Possible values are <code>DownFlowLayout.TOP</code>,
   * <code>DownFlowLayout.BOTTOM</code>, and <code>DownFlowLayout.CENTER</code>.
   * @param      align the alignment value.
   * @see        DownFlowLayout#getAlignment
   * @since      JDK1.1
   */
  public void setAlignment(int align) {
    this.align = align;
  }

  /**
   * Gets the horizontal gap between components.
   * @return     the horizontal gap between components.
   * @see        DownFlowLayout#setHgap
   * @since      JDK1.1
   */
  public int getHgap() {
    return hgap;
  }

  /**
   * Sets the horizontal gap between components.
   * @param hgap the horizontal gap between components
   * @see        DownFlowLayout#getHgap
   * @since      JDK1.1
   */
  public void setHgap(int hgap) {
    this.hgap = hgap;
  }

  /**
   * Gets the vertical gap between components.
   * @return     the vertical gap between components.
   * @see        DownFlowLayout#setVgap
   * @since      JDK1.1
   */
  public int getVgap() {
    return vgap;
  }

  /**
   * Sets the vertical gap between components.
   * @param vgap the vertical gap between components
   * @see        DownFlowLayout#getVgap
   * @since      JDK1.1
   */
  public void setVgap(int vgap) {
    this.vgap = vgap;
  }

  /**
   * Adds the specified component to the layout. Not used by this class.
   * @param name the name of the component
   * @param comp the component to be added
   * @since JDK1.0
   */
  public void addLayoutComponent(String name, Component comp) {
  }

  /**
   * Removes the specified component from the layout. Not used by
   * this class.
   * @param comp the component to remove
   * @see       java.awt.Container#removeAll
   * @since     JDK1.0
   */
  public void removeLayoutComponent(Component comp) {
  }

  /**
   * Returns the preferred dimensions for this layout given the components
   * in the specified target container.
   * @param target the component which needs to be laid out
   * @return    the preferred dimensions to lay out the
   *                    subcomponents of the specified container.
   * @see Container
   * @see #minimumLayoutSize
   * @see       java.awt.Container#getPreferredSize
   * @since     JDK1.0
   */
  public Dimension preferredLayoutSize(Container target) {
    synchronized (target.getTreeLock()) {

      Dimension dim = new Dimension(0, 0);
      int nmembers = target.getComponentCount();

      for (int i = 0; i < nmembers; i++) {
        Component m = target.getComponent(i);
        if (m.isVisible()) {
          Dimension d = m.getPreferredSize();
          dim.width = Math.max(dim.width, d.width);
          if (i > 0) {
            dim.height += vgap;
          }
          dim.height += d.height;
        }
      }
      Insets insets = target.getInsets();
      dim.width += insets.left + insets.right + hgap * 2;
      dim.height += insets.top + insets.bottom + vgap * 2;
      return dim;
    }

  }

  /**
   * Returns the minimum dimensions needed to layout the components
   * contained in the specified target container.
   * @param target the component which needs to be laid out
   * @return    the minimum dimensions to lay out the
   *                    subcomponents of the specified container.
   * @see #preferredLayoutSize
   * @see       java.awt.Container
   * @see       java.awt.Container#doLayout
   * @since     JDK1.0
   */
  public Dimension minimumLayoutSize(Container target) {
    synchronized (target.getTreeLock()) {

      Dimension dim = new Dimension(0, 0);
      int nmembers = target.getComponentCount();

      for (int i = 0; i < nmembers; i++) {
        Component m = target.getComponent(i);
        if (m.isVisible()) {
          Dimension d = m.getMinimumSize();
          dim.width = Math.max(dim.width, d.width);
          if (i > 0) {
            dim.height += vgap;
          }
          dim.height += d.height;
        }
      }
      Insets insets = target.getInsets();
      dim.width += insets.left + insets.right + hgap * 2;
      dim.height += insets.top + insets.bottom + vgap * 2;
      return dim;
    }

  }

  /**
   * Centers the elements in the specified row, if there is any slack.
   * @param target the component which needs to be moved
   * @param x the x coordinate
   * @param y the y coordinate
   * @param width the width dimensions
   * @param height the height dimensions
   * @param columnStart the beginning of the column
   * @param columnEnd the the ending of the column
   */
  private void moveComponents(Container target, int x, int y, int width, int height, int columnStart, int columnEnd) {
    synchronized (target.getTreeLock()) {

      switch (align) {
        case TOP:
          break;
        case CENTER:
          y += height / 2;
          break;
        case BOTTOM:
          y += height;
          y += height;
          break;
      }
      for (int i = columnStart; i < columnEnd; i++) {
        Component m = target.getComponent(i);
        if (m.isVisible()) {
          Dimension mdim = m.getSize();
          m.setLocation(x + (width - mdim.width) / 2, y);
          y += vgap + mdim.height;
        }
      }
    }

  }

  /**
   * Lays out the container. This method lets each component take
   * its preferred size by reshaping the components in the
   * target container in order to satisfy the constraints of
   * this <code>DownFlowLayout</code> object.
   * @param target the specified component being laid out.
   * @see Container
   * @see       java.awt.Container#doLayout
   * @since     JDK1.0
   */
  public void layoutContainer(Container target) {
    synchronized (target.getTreeLock()) {

      Insets insets = target.getInsets();
      Dimension tdim = target.getSize();
      int maxheight = tdim.height - (insets.top + insets.bottom + vgap * 2);
      int nmembers = target.getComponentCount();
      int x = insets.left + hgap, y = 0;
      int columnw = 0, start = 0;

      for (int i = 0; i < nmembers; i++) {
        Component m = target.getComponent(i);
        if (m.isVisible()) {
          Dimension d = m.getPreferredSize();
          m.setSize(d.width, d.height);

          if ((y == 0) || ((y + d.height) <= maxheight)) {
            if (y > 0) {
              y += vgap;
            }
            y += d.height;
            columnw = Math.max(columnw, d.width);
          } else {
            moveComponents(target, x, insets.top + vgap, columnw, maxheight - y, start, i);
            y = d.height;
            x += hgap + columnw;
            columnw = d.width;
            start = i;
          }
        }
      }
      moveComponents(target, x, insets.top + vgap, columnw, maxheight - y, start, nmembers);
    }

  }

  /**
   * Returns a string representation of this <code>DownFlowLayout</code>
   * object and its values.
   * @return     a string representation of this layout.
   * @since      JDK1.0
   */
  public String toString() {
    String str = "";
    switch (align) {
      case TOP:
        str = ",align=top";
        break;
      case CENTER:
        str = ",align=vertical center";
        break;
      case BOTTOM:
        str = ",align=bottom";
        break;
    }
    return getClass().getName() + "[hgap=" + hgap + ",vgap=" + vgap + str + "]";
  }
}
