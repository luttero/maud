/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot.gui;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Toolkit;
import javax.swing.JFrame;
import javax.swing.JTabbedPane;
import javax.swing.UIManager;

/**
 *
 * @author hios
 */
public class App {

    public static void main(String[] args) {
        new App();
    }

    SimpleApp data;
    JFrame frame;
    SimpleMenu menubar;

    public App() {
        installLookAndFeel();
        data = new SimpleApp();

        JTabbedPane pane = new JTabbedPane();
        menubar = new SimpleMenu(data, pane);

        frame = createFrame();
        Container contentPane = frame.getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(pane);
        contentPane.add(menubar, "North");
        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        frame.setVisible(true);

    }

    private void installLookAndFeel() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception ex) {
//            Logger.getLogger(ServerFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private JFrame createFrame() {
        JFrame frame = new JFrame();
        Dimension sz = Toolkit.getDefaultToolkit().getScreenSize();
        sz.height -= 150;
        sz.width -= 100;
        frame.setSize(sz);
        frame.setLocation(50, 50);
        frame.setTitle("JTEX");
        return frame;
    }

}
