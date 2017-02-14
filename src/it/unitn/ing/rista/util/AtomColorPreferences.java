/*
 * @(#)AtomColorPreferences.java created Mar 28, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.util;

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.models.prefTableModel;
import it.unitn.ing.rista.interfaces.PreferencesInterface;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.table.TableCellRenderer;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;


/**
 * The AtomColorPreferences is a class
 *
 * @version $Revision: 1.7 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AtomColorPreferences extends PreferencesInterface {

  static String[][] defaultColors = {{"Ac", "122,196,146,255"},
                                     {"Al", "187,231,173,255"},
                                     {"Am", "126,99,85,255"},
                                     {"Sb", "92,212,181,255"},
                                     {"Ar", "226,187,225,255"},
                                     {"As", "110,137,135,255"},
                                     {"At", "111,31,152,255"},
                                     {"Au", "243,252,160,255"},
                                     {"B", "174,83,70,255"},
                                     {"Ba", "180,113,148,255"},
                                     {"Be", "94,137,237,255"},
                                     {"Bh", "235,109,63,255"},
                                     {"Bi", "167,56,166,255"},
                                     {"Bk", "165,119,17,255"},
                                     {"Br", "47,10,205,255"},
                                     {"C", "193,160,239,255"},
                                     {"Ca", "63,18,213,255"},
                                     {"Cd", "79,246,247,255"},
                                     {"Ce", "47,205,70,255"},
                                     {"Cf", "48,170,77,255"},
                                     {"Cl", "253,80,101,255"},
                                     {"Cm", "147,105,99,255"},
                                     {"Co", "69,212,110,255"},
                                     {"Cr", "102,54,176,255"},
                                     {"Cs", "115,75,189,255"},
                                     {"Cu", "116,124,241,255"},
                                     {"Db", "141,207,119,255"},
                                     {"Dy", "1,73,101,255"},
                                     {"Er", "227,185,227,255"},
                                     {"Es", "238,117,65,255"},
                                     {"Eu", "53,19,34,255"},
                                     {"F", "103,57,82,255"},
                                     {"Fe", "63,128,168,255"},
                                     {"Fm", "236,186,192,255"},
                                     {"Fr", "108,137,8,255"},
                                     {"Ga", "152,201,98,255"},
                                     {"Gd", "90,115,160,255"},
                                     {"Ge", "108,117,166,255"},
                                     {"H", "184,143,122,255"},
                                     {"He", "247,19,207,255"},
                                     {"Hf", "107,84,117,255"},
                                     {"Hg", "56,162,238,255"},
                                     {"Ho", "76,156,16,255"},
                                     {"Hs", "99,173,188,255"},
                                     {"I", "89,183,125,255"},
                                     {"In", "67,141,40,255"},
                                     {"Ir", "235,179,165,255"},
                                     {"K", "27,53,19,255"},
                                     {"Kr", "30,143,23,255"},
                                     {"La", "91,159,228,255"},
                                     {"Li", "83,9,85,255"},
                                     {"Lr", "255,143,228,255"},
                                     {"Lu", "66,237,208,255"},
                                     {"Md", "231,49,68,255"},
                                     {"Mg", "137,152,229,255"},
                                     {"Mn", "169,99,130,255"},
                                     {"Mo", "144,146,163,255"},
                                     {"Mt", "144,163,42,255"},
                                     {"N", "188,143,135,255"},
                                     {"Na", "252,191,108,255"},
                                     {"Nb", "230,20,187,255"},
                                     {"Nd", "149,57,155,255"},
                                     {"Ne", "160,31,110,255"},
                                     {"Ni", "42,203,194,255"},
                                     {"No", "2,71,71,255"},
                                     {"Np", "234,12,91,255"},
                                     {"O", "156,154,103,255"},
                                     {"Os", "90,116,157,255"},
                                     {"P", "230,174,214,255"},
                                     {"Pa", "70,129,130,255"},
                                     {"Pb", "61,6,172,255"},
                                     {"Pd", "72,243,9,255"},
                                     {"Pm", "14,52,75,255"},
                                     {"Po", "200,18,144,255"},
                                     {"Pr", "195,230,65,255"},
                                     {"Pt", "145,143,148,255"},
                                     {"Pu", "22,152,168,255"},
                                     {"Ra", "132,234,224,255"},
                                     {"Rb", "231,169,106,255"},
                                     {"Re", "253,177,123,255"},
                                     {"Rf", "200,216,162,255"},
																		 {"Y", "128,154,65,255"}};


	public static Preferences prefs;

  public AtomColorPreferences() {
  }

  public static void loadPreferences() {

	  prefs = Preferences.userRoot().node(AtomColorPreferences.class.getName());

    for (int i = 0; i < 82; i++) {
	    String key = "Color." + defaultColors[i][0];
	    if (!contains(key))
		    prefs.put(key, defaultColors[i][1]);
	    prefs.get(key, defaultColors[i][1]);
    }
  }

	public static boolean contains(String key) {
		return prefs.get(key, null) != null;
	}

	public Object getValue(String key) {
    return AtomColorPreferences.getColor(key);
  }

  public void setValue(String key, Object value) {
	  if (value instanceof Color)
		  setPref(key, (Color) value);
	  else
      prefs.put(key, value.toString());
  }

  public static Color getColor(String key) {
    return stringToColor(prefs.get(key, "128,65,65,255"));
  }

  public static void setPref(String key, String value) {
    prefs.put(key, value);
  }

  public static void setPref(String key, Color value) {
    prefs.put(key, colorToString(value));
  }

  private static Color stringToColor(String color) {
    StringTokenizer str_tok = new StringTokenizer(color, ",");
    int r = Integer.parseInt(str_tok.nextToken());
    int g = Integer.parseInt(str_tok.nextToken());
    int b = Integer.parseInt(str_tok.nextToken());
    int a = Integer.parseInt(str_tok.nextToken());
    return new Color(r, g, b, a);
  }

  private static String colorToString(Color color) {
    return new String(color.getRed() + "," + color.getGreen() + "," + color.getBlue() + "," + color.getAlpha());
  }

  static myJFrame prefDialog = null;

  public static void showPrefs(Frame aframe) {
    if (prefDialog == null) {
      prefDialog = new ColorPreferencesDialog(aframe, "AtomColor preferences");
    }
    prefDialog.setVisible(true);
  }

  static class ColorPreferencesDialog extends myJFrame {

    prefTableModel prefModel = null;

    public ColorPreferencesDialog(Frame parent, String title) {
      super(parent, title);

      frameWLabel = "ColorPreferencesDialog.frameWidth";
      frameHLabel = "ColorPreferencesDialog.frameHeight";
      defaultFrameW = 500;
      defaultFrameH = 300;
      setOwnSize = true;
      framePositionX = "ColorPreferencesDialog.framePositionX";
      framePositionY = "ColorPreferencesDialog.framePositionY";
      defaultFramePositionX = 100;
      defaultFramePositionY = 20;
      setOwnPosition = true;

      setDefaultCloseOperation(HIDE_ON_CLOSE);

      Container c1 = getContentPane();
      c1.setLayout(new BorderLayout(6, 6));

      prefModel = new prefTableModel(AtomColorPreferences.prefs, new AtomColorPreferences());

      JTable preftable = new JTable(prefModel);

      setUpColorRenderer(preftable);
      setUpColorEditor(preftable);
      JScrollPane scrollpane = new JScrollPane(preftable);
      c1.add(scrollpane, BorderLayout.CENTER);
      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));

      JCloseButton btnClose = new JCloseButton();
      btnClose.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
        }
      });
      jp.add(btnClose);

      c1.add(jp, BorderLayout.SOUTH);
      getRootPane().setDefaultButton(btnClose);
    }

    public void setVisible(boolean b) {
      prefModel.refreshTable();
      super.setVisible(b);
    }


    class ColorRenderer extends JLabel
            implements TableCellRenderer {
      Border unselectedBorder = null;
      Border selectedBorder = null;
      boolean isBordered = true;

      public ColorRenderer(boolean isBordered) {
        super();
        this.isBordered = isBordered;
        setOpaque(true); //MUST do this for background to show up.
      }

      public Component getTableCellRendererComponent(
              JTable table, Object color,
              boolean isSelected, boolean hasFocus,
              int row, int column) {
        setBackground((Color) color);
        if (isBordered) {
          if (isSelected) {
            if (selectedBorder == null) {
              selectedBorder = BorderFactory.createMatteBorder(2, 5, 2, 5,
                      table.getSelectionBackground());
            }
            setBorder(selectedBorder);
          } else {
            if (unselectedBorder == null) {
              unselectedBorder = BorderFactory.createMatteBorder(2, 5, 2, 5,
                      table.getBackground());
            }
            setBorder(unselectedBorder);
          }
        }
        return this;
      }
    }

    private void setUpColorRenderer(JTable table) {
      table.setDefaultRenderer(Color.class, new ColorRenderer(true));
    }

    //Set up the editor for the Color cells.
    private void setUpColorEditor(JTable table) {
      //First, set up the button that brings up the dialog.
      final JButton button = new JButton("") {
        public void setText(String s) {
          //Button never shows text -- only color.
        }
      };
      button.setBackground(Color.white);
      button.setBorderPainted(false);
      button.setMargin(new Insets(0, 0, 0, 0));

      //Now create an editor to encapsulate the button, and
      //set it up as the editor for all Color cells.
      final ColorEditor colorEditor = new ColorEditor(button);
      table.setDefaultEditor(Color.class, colorEditor);

      //Set up the dialog that the button brings up.
      final JColorChooser colorChooser = new JColorChooser();
      ActionListener okListener = new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          colorEditor.currentColor = colorChooser.getColor();
        }
      };
      final JDialog dialog = JColorChooser.createDialog(button,
              "Pick a Color",
              true,
              colorChooser,
              okListener,
              null); //XXXDoublecheck this is OK

      //Here's the code that brings up the dialog.
      button.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          button.setBackground(colorEditor.currentColor);
          colorChooser.setColor(colorEditor.currentColor);
          //Without the following line, the dialog comes up
          //in the middle of the screen.
          //dialog.setLocationRelativeTo(button);
          dialog.show();
        }
      });
    }

    /*
     * The editor button that brings up the dialog.
     * We extend DefaultCellEditor for convenience,
     * even though it mean we have to create a dummy
     * check box.  Another approach would be to copy
     * the implementation of TableCellEditor methods
     * from the source code for DefaultCellEditor.
     */
    class ColorEditor extends DefaultCellEditor {
      Color currentColor = null;

      public ColorEditor(JButton b) {
        super(new JCheckBox()); //Unfortunately, the constructor
        //expects a check box, combo box,
        //or text field.
        editorComponent = b;
        setClickCountToStart(1); //This is usually 1 or 2.

        //Must do this so that editing stops when appropriate.
        b.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            fireEditingStopped();
          }
        });
      }

      protected void fireEditingStopped() {
        super.fireEditingStopped();
      }

      public Object getCellEditorValue() {
        return currentColor;
      }

      public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
        ((JButton) editorComponent).setText(value.toString());
        currentColor = (Color) value;
        return editorComponent;
      }
    }
  }


}

