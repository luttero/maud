/*
 * @(#)Utility.java created Jan 15, 2006 Casalino
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

package it.unitn.ing.rista.awt;

import it.unitn.ing.rista.models.prefTableModel;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import javax.swing.table.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;


/**
 * The Utility is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/07/20 13:39:03 $
 * @since JDK1.1
 */

public class Utility {

  public static void centerOnScreen(Frame aframe) {
    putOnScreenAt(aframe, 50, 50);
  }

  public static void centerOnScreen(Dialog aframe) {
    putOnScreenAt(aframe, 50, 50);
  }

  public static void putOnScreenAt(Frame aframe, int percent) {
    putOnScreenAt(aframe, percent, percent);
  }

  public static void putOnScreenAt(Dialog aframe, int percent) {
    putOnScreenAt(aframe, percent, percent);
  }

  public static void putOnScreenAt(Frame aframe, int xpercent, int ypercent) {
    Dimension paneSize = aframe.getSize();
    Dimension screenSize = aframe.getToolkit().getScreenSize();

    aframe.setLocation((screenSize.width - paneSize.width) * xpercent / 100,
        (screenSize.height - paneSize.height) * ypercent / 100);
  }

  public static void putOnScreenAt(Dialog aframe, int xpercent, int ypercent) {
    Dimension paneSize = aframe.getSize();
    Dimension screenSize = aframe.getToolkit().getScreenSize();

    aframe.setLocation((screenSize.width - paneSize.width) * xpercent / 100,
        (screenSize.height - paneSize.height) * ypercent / 100);
  }

  public static void centerOnFrame(Frame aframe, Frame principalFrame) {
    Dimension paneSize = aframe.getSize();
    Dimension principalPaneSize = principalFrame.getSize();
    Point principalPaneLocation = principalFrame.getLocation();

    aframe.setLocation(principalPaneLocation.x + (principalPaneSize.width - paneSize.width) / 2,
        principalPaneLocation.y + (principalPaneSize.height - paneSize.height) / 2);
  }

  public static void centerOnFrame(Dialog aframe, Frame principalFrame) {
    Dimension paneSize = aframe.getSize();
    Dimension principalPaneSize = principalFrame.getSize();
    Point principalPaneLocation = principalFrame.getLocation();

    aframe.setLocation(principalPaneLocation.x + (principalPaneSize.width - paneSize.width) / 2,
        principalPaneLocation.y + (principalPaneSize.height - paneSize.height) / 2);
  }

  public static String[] browseFilenames(Frame parent, String title) {
	  String[] filenames = null;
		if (System.getProperty("java.version").compareTo("1.7") < 0) {
			filenames = new String[1];
			filenames[0] = browseFilename(parent, title);
		} else {

			FileDialog fileDialog = new FileDialog(parent, title, FileDialog.LOAD);
			fileDialog.setMultipleMode(true);
			fileDialog.setVisible(true);
			File files[] = fileDialog.getFiles();
			if (files != null && files.length > 0) {
				filenames = new String[files.length];
				for (int i = 0; i < files.length; i++)
					filenames[i] = files[i].getAbsolutePath();
				String[] folderAndName = Misc.getFolderandName(filenames[0]);
				MaudPreferences.setPref(principalJFrame.datafilePath, folderAndName[0]);
			}
			fileDialog.dispose();
		}
    return filenames;
  }

  public static String browseFilename(Frame parent, String title) {
    String filename = openFileDialog(parent, title, FileDialog.LOAD,
        MaudPreferences.getPref(principalJFrame.datafilePath, Constants.documentsDirectory),
        null, null);
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref(principalJFrame.datafilePath,
          folderAndName[0]);
    }
    return filename;
  }

	public static String browseFilenametoSave(Frame parent, String title) {
    String filename = openFileDialog(parent, title, FileDialog.SAVE,
		    MaudPreferences.getPref(principalJFrame.datafilePath, Constants.documentsDirectory), null, null);
    if (filename != null) {
      String[] folderAndName = Misc.getFolderandName(filename);
      MaudPreferences.setPref(principalJFrame.datafilePath,
          folderAndName[0]);
    }
    return filename;
  }

  public static String browseFilenameForAppend(String title, String folder_filename) {
  	if (Constants.macosx) {
      return browseFilename(null, title);
    }
    final String description = title;
    try {
      File file = new File(folder_filename);
      JFileChooser fc = new JFileChooser(file);
      fc.setFileFilter(new javax.swing.filechooser.FileFilter() {
        public boolean accept(File f) {
          return f.isDirectory();
        }

        public String getDescription() {
          return description;
        }
      });
      if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
//        fc.getSelectedFile().getCanonicalPath();
        return fc.getSelectedFile().getCanonicalPath();
      }
    } catch (SecurityException ex) {
      ex.printStackTrace();
    } catch (Exception ex) {
      ex.printStackTrace();
    }
    return null;
  }

	public static String openFileDialogForLoad(String title, String preferredPath, String filenameToLoad) {
		if (Constants.testing)
			System.out.println("Call open file dialog to load: " + preferredPath + " : " + filenameToLoad);
		return openFileDialog(new Frame(), title, FileDialog.LOAD, preferredPath, null, filenameToLoad);
	}

	public static String openFileDialog(Frame frameParent, String title, int loading,
                                      String preferredPath, String extensionFilter, String filenameForSave) {
    String filename = null;
    FileDialog fd = new FileDialog(frameParent, title, loading);
    if (extensionFilter != null && !extensionFilter.equals("")) {
    }
    if (preferredPath != null)
      fd.setDirectory(Misc.checkForWindowsPath(preferredPath));
    if (filenameForSave != null)
      fd.setFile(filenameForSave);
    fd.setVisible(true);
    if (fd.getFile() != null) {
      filename = Misc.filterFileName(fd.getDirectory() + fd.getFile());
    }
    fd.dispose();
    return filename;
  }

  static boolean result = true;

  public static boolean areYouSureToRemove(String message) {
    JButton removeButton = new JIconButton("Check.gif", "Remove");
    result = false;

    final AttentionD attdlg = new AttentionD(null,
        message, true, removeButton, true);
    removeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent event) {
        result = true;
        attdlg.setVisible(false);
        attdlg.dispose();
      }
    });
    attdlg.setVisible(true);

    while (attdlg.isVisible()) {
      try {
        Thread.sleep(100);
      } catch (InterruptedException ie) {
        // do nothing here
      }

    }

    return result;
  }

  public static Frame getParentFrame(Container comp) {
    Container parent = comp;
    while (parent.getParent() != null && !(parent instanceof Frame)) {
      parent = parent.getParent();
    }
    return (Frame) parent;
  }

  static myJFrame prefDialog = null;

  public static void showPrefs(Frame aframe) {
    if (prefDialog == null) {
      prefDialog = new PreferencesDialog(aframe, "Preferences");
    }
    prefDialog.setVisible(true);
  }

  static myJFrame parPrefDialog = null;

  public static void showParameterPrefs(Frame aframe) {
    if (parPrefDialog == null) {
      parPrefDialog = new ParPreferencesDialog(aframe, "Parameters preferences");
    }
    parPrefDialog.setVisible(true);
  }

  static class PreferencesDialog extends myJFrame {

    private DefaultCellEditor booleanEditor;
    prefTableModel prefModel = null;

    public PreferencesDialog(Frame parent, String title) {
      super(parent, title);

      frameWLabel = "preferencesFrame.frameWidth";
      frameHLabel = "preferencesFrame.frameHeight";
      defaultFrameW = 500;
      defaultFrameH = 300;
      setOwnSize = true;
      framePositionX = "preferencesFrame.framePositionX";
      framePositionY = "preferencesFrame.framePositionY";
      defaultFramePositionX = 100;
      defaultFramePositionY = 20;
      setOwnPosition = true;

      setDefaultCloseOperation(HIDE_ON_CLOSE);

      Container c1 = getContentPane();
      c1.setLayout(new BorderLayout(6, 6));
      JComboBox boolComboBox = new JComboBox();
      boolComboBox.addItem("true");
      boolComboBox.addItem("false");
      booleanEditor = new DefaultCellEditor(boolComboBox);

      prefModel = new prefTableModel(MaudPreferences.prefs, new MaudPreferences());
      JTable preftable = new JTable(prefModel) {
/*        public TableCellRenderer getCellRenderer(int row, int column) {
          String obj = (String) prefModel.getValueAt(row, column);
          if (obj.equalsIgnoreCase("false") || obj.equalsIgnoreCase("true")) {
            return colorRenderer;
          } else if ((row == 2) && (column == 1)) {
            return fileRenderer;
          }
          return super.getCellRenderer(row, column);
        }*/

        public TableCellEditor getCellEditor(int row, int column) {
          String obj = (String) prefModel.getValueAt(row, column);
          if (column == 1 && (obj.equalsIgnoreCase("false") || obj.equalsIgnoreCase("true"))) {
            return booleanEditor;
          }
          return super.getCellEditor(row, column);
        }
      };
      JScrollPane scrollpane = new JScrollPane(preftable);
      c1.add(scrollpane, BorderLayout.CENTER);
      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      JButton jbs = new JButton("Save on disk");
      jbs.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          Constants.checkMaudPreferences();
          setVisible(false);
        }
      });
      jp.add(jbs);
      JCloseButton jb = new JCloseButton();
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          Constants.checkMaudPreferences();
          setVisible(false);
        }
      });
      jp.add(jb);
      c1.add(jp, BorderLayout.SOUTH);
      getRootPane().setDefaultButton(jb);
    }

    public void setVisible(boolean b) {
      prefModel.refreshTable();
      super.setVisible(b);
    }

  }

  static class ParPreferencesDialog extends myJFrame {

    prefTableModel prefModel = null;

    public ParPreferencesDialog(Frame parent, String title) {
      super(parent, title);

      frameWLabel = "parPreferencesFrame.frameWidth";
      frameHLabel = "parPreferencesFrame.frameHeight";
      defaultFrameW = 500;
      defaultFrameH = 300;
      setOwnSize = true;
      framePositionX = "parPreferencesFrame.framePositionX";
      framePositionY = "parPreferencesFrame.framePositionY";
      defaultFramePositionX = 100;
      defaultFramePositionY = 20;
      setOwnPosition = true;

      setDefaultCloseOperation(HIDE_ON_CLOSE);

      Container c1 = getContentPane();
      c1.setLayout(new BorderLayout(6, 6));

      prefModel = new prefTableModel(ParameterPreferences.prefs, new ParameterPreferences());
      JTable preftable = new JTable(prefModel);
      JScrollPane scrollpane = new JScrollPane(preftable);
      c1.add(scrollpane, BorderLayout.CENTER);
      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      JButton jbs = new JButton("Save on disk");
      jbs.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
        }
      });
      jp.add(jbs);
      JCloseButton jb = new JCloseButton();
      jb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent event) {
          setVisible(false);
        }
      });
      jp.add(jb);
      c1.add(jp, BorderLayout.SOUTH);
      getRootPane().setDefaultButton(jb);
    }

    public void setVisible(boolean b) {
      prefModel.refreshTable();
      super.setVisible(b);
    }

  }

/* example for different editors in different rows

  class HTMLTableEditor extends JFrame {
    private JTable table;
    private HTMLTableModel tableModel;
    private ColorRenderer colorRenderer;
    private ColorEditor colorEditor;
    private FileEditor fileEditor;
    private FileRenderer fileRenderer;
    private DefaultCellEditor booleanEditor;

    public HTMLTableEditor() {
      fileEditor = new FileEditor();
      fileRenderer = new FileRenderer(true);
      JComboBox boolComboBox = new JComboBox();
      boolComboBox.addItem("True");
      boolComboBox.addItem("False");

      booleanEditor = new DefaultCellEditor(boolComboBox);
      colorRenderer = new ColorRenderer(true);
      colorEditor = new ColorEditor();
      tableModel = new HTMLTableModel();
      table = new JTable(tableModel) {
        public TableCellRenderer getCellRenderer(int row, int column) {
          if ((row == 0) && (column == 1)) {
            return colorRenderer;
          } else if ((row == 2) && (column == 1)) {
            return fileRenderer;
          }
          return super.getCellRenderer(row, column);
        }

        public TableCellEditor getCellEditor(int row, int column) {
          if ((row == 0) && (column == 1)) {
            return colorEditor;
          } else if ((row == 1) && (column == 1)) {
            return booleanEditor;
          } else if ((row == 2) && (column == 1)) {
            return fileEditor;
          }
          return super.getCellEditor(row, column);
        }
      };
      table.setRowHeight(2, 200);
      this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      this.getContentPane().add(new JScrollPane(table), BorderLayout.CENTER);
      this.pack();
      this.setVisible(true);
      this.setTitle("HTML Editor");
    }


    class HTMLTableModel extends AbstractTableModel {
      private String[] columnNames = {"Name",
          "Value"};
      private Object[][] data =
          {
              {"Background-Color", new Color(0, 0, 128)},
              {"isTextWrap", new Boolean(true)},
              {"Background-Image", new String("c:/windows/desktop/hypercard.png")}
          };

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return data.length;
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        return data[row][col];
      }

      public Class getColumnClass(int c) {
        return getValueAt(0, c).getClass();
      }

      public boolean isCellEditable(int row, int col) {
        if (col < 1) {
          return false;
        } else {
          return true;
        }
      }

      public void setValueAt(Object value, int row, int col) {
        data[row][col] = value;
        fireTableCellUpdated(row, col);
      }
    }

    public class FileEditor extends AbstractCellEditor
        implements TableCellEditor,
        ActionListener {
      String currentFile;
      JButton button;
      JFileChooser fileChooser;
      protected static final String EDIT = "edit";

      public FileEditor() {
        button = new JButton();
        button.setActionCommand(EDIT);
        button.addActionListener(this);
        button.setBorderPainted(false);
        fileChooser = new JFileChooser();
      }

      / **
       * Handles events from the editor button and from
       * the dialog's OK button.
       * /
      public void actionPerformed(ActionEvent e) {
        if (EDIT.equals(e.getActionCommand())) {
          button.setText(currentFile);
//The user has clicked the cell, so
//bring up the dialog.
          int returnVal = fileChooser.showOpenDialog(HTMLTableEditor.this);
          if (returnVal == JFileChooser.APPROVE_OPTION) {
            try {
              currentFile = fileChooser.getSelectedFile().getCanonicalPath();
            }
            catch (IOException ie) {
            }
          }
//Make the renderer reappear.
          fireEditingStopped();
        }
      }

      //Implement the one CellEditor method that AbstractCellEditor doesn't.
      public Object getCellEditorValue() {
        return currentFile;
      }

      //Implement the one method defined by TableCellEditor.
      public Component getTableCellEditorComponent(JTable table,
                                                   Object value,
                                                   boolean isSelected,
                                                   int row,
                                                   int column) {
        currentFile = (String) value;
        return button;
      }
    }


    public class ColorEditor extends AbstractCellEditor
        implements TableCellEditor,
        ActionListener {
      Color currentColor;
      JButton button;
      JColorChooser colorChooser;
      JDialog dialog;
      protected static final String EDIT = "edit";

      public ColorEditor() {
        button = new JButton();
        button.setActionCommand(EDIT);
        button.addActionListener(this);
        button.setBorderPainted(false);

        colorChooser = new JColorChooser();
        dialog = JColorChooser.createDialog(button,
            "Pick a Color",
            true, //modal
            colorChooser,
            this, //OK button handler
            null); //no CANCEL button handler
      }

      / **
       * Handles events from the editor button and from
       * the dialog's OK button.
       * /
      public void actionPerformed(ActionEvent e) {
        if (EDIT.equals(e.getActionCommand())) {
//The user has clicked the cell, so
//bring up the dialog.
          button.setBackground(currentColor);
          colorChooser.setColor(currentColor);
          dialog.setVisible(true);

//Make the renderer reappear.
          fireEditingStopped();

        } else {
//User pressed dialog's "OK" button.
          currentColor = colorChooser.getColor();
        }
      }

      //Implement the one CellEditor method that AbstractCellEditor doesn't.
      public Object getCellEditorValue() {
        return currentColor;
      }

      //Implement the one method defined by TableCellEditor.
      public Component getTableCellEditorComponent(JTable table,
                                                   Object value,
                                                   boolean isSelected,
                                                   int row,
                                                   int column) {
        currentColor = (Color) value;
        return button;
      }
    }

    public class ColorRenderer extends JButton
        implements TableCellRenderer {
      Border unselectedBorder = null;
      Border selectedBorder = null;
      boolean isBordered = true;

      public ColorRenderer(boolean isBordered) {
        this.isBordered = isBordered;
        setOpaque(true);
      }

      public Component getTableCellRendererComponent(
          JTable table, Object color,
          boolean isSelected, boolean hasFocus,
          int row, int column) {
        Color newColor = (Color) color;
        this.setIcon(new MiniIcon(newColor));
        this.setText("(" + newColor.getRed() + " " +
            newColor.getGreen() + " " +
            newColor.getBlue() + ")");
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

    public class MiniIcon implements Icon {
      private Color color;

      public MiniIcon(Color c) {
        color = c;
      }

      public int getIconHeight() {
        return 16;
      }

      public int getIconWidth() {
        return 16;
      }

      public void paintIcon(Component c, Graphics g, int x, int y) {
        Graphics2D g2 = (Graphics2D) g;
        g2.setColor(color);
        g2.fill(new Rectangle(x, y, 16, 16));
      }
    }

    public class FileRenderer extends JButton
        implements TableCellRenderer {
      Border unselectedBorder = null;
      Border selectedBorder = null;
      boolean isBordered = true;

      public FileRenderer(boolean isBordered) {
        this.isBordered = isBordered;
//setOpaque(true); //MUST do this for background to show up.
      }

      public Component getTableCellRendererComponent(
          JTable table, Object file,
          boolean isSelected, boolean hasFocus,
          int row, int column) {
        ImageIcon image = new ImageIcon((String) file);
        setIcon(image);
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
        setToolTipText((String) file);
        return this;
      }
    }
  } */

}