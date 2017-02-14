package it.unitn.ing.jgraph;

import java.awt.*;
import java.lang.*;

/**
 * A structure class used exclusively with the TextLine class.
 * When the Text changes state (new font, new color, new offset)
 * then this class holds the information plus the substring
 * that the state pertains to.
 */
class TextState extends Object {
  Font f = null;
  StringBuffer s = null;
  int x = 0;
  int y = 0;


  public TextState() {
    s = new StringBuffer();
  }


  public TextState copyAll() {
    TextState tmp = copyState();
    if (s.length() == 0) return tmp;
    for (int i = 0; i < s.length(); i++) {
      tmp.s.append(s.charAt(i));
    }
    return tmp;
  }


  public TextState copyState() {
    TextState tmp = new TextState();
    tmp.f = f;
    tmp.x = x;
    tmp.y = y;
    return tmp;
  }


  public String toString() {
    return s.toString();
  }


  public boolean isEmpty() {
    return (s.length() == 0);
  }

  public int getWidth(Graphics g) {

    if (g == null || f == null || s.length() == 0) return 0;

    return g.getFontMetrics(f).stringWidth(s.toString());
  }

  public int getHeight(Graphics g) {
    if (g == null || f == null) return 0;

    return g.getFontMetrics(f).getHeight();
  }

  public int getAscent(Graphics g) {
    if (g == null || f == null) return 0;

    return g.getFontMetrics(f).getAscent();
  }

  public int getDescent(Graphics g) {
    if (g == null || f == null) return 0;

    return g.getFontMetrics(f).getDescent();
  }

  public int getMaxAscent(Graphics g) {
    if (g == null || f == null) return 0;

    return g.getFontMetrics(f).getMaxAscent();
  }

  public int getMaxDescent(Graphics g) {
    if (g == null || f == null) return 0;

    return g.getFontMetrics(f).getMaxDescent();
  }

  public int getLeading(Graphics g) {
    if (g == null || f == null) return 0;

    return g.getFontMetrics(f).getLeading();
  }
}

