package org.javadev.effects;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.awt.image.*;


/**
 * The SlideAnimation is a class to animate the transition between
 * one component and the other for AnimatingCardLayout. Based on the Dashboard
 * code of Dmitry Markman.
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @author Dmitry Markman
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:46:00 $
 * @since JDK1.1
 */

public class SlideAnimation implements Animation {

  public static final int NORTH = 0;
  public static final int SOUTH = 1;
  public static final int EAST = 2;
  public static final int WEST = 3;
  public static final boolean UPPER = true;
  public static final boolean LOWER = false;

  SpecialPanel animationPanel = null;
  private AnimationListener listener = null;
  boolean direction = true;
  boolean movingUpperComponent = true;
  int movingDirection = WEST;
  int animationDuration = 2000;

  public SlideAnimation() {
    this(UPPER, WEST);
  }

  public SlideAnimation(boolean movingUpperComp) {
    this(movingUpperComp, WEST);
  }

  public SlideAnimation(int direction) {
    this(UPPER, direction);
  }

  public SlideAnimation(int direction, boolean movingUpperComp) {
    this(movingUpperComp, direction);
  }

  public SlideAnimation(boolean movingUpperComp, int direction) {
    movingDirection = direction;
    movingUpperComponent = movingUpperComp;
  }

  public void setDirection(boolean direction) {
    this.direction = direction;
  }

  public void setComponentToMove(boolean movingUpper) {
    movingUpperComponent = movingUpper;
  }

  public void setAnimationDuration(int animationDuration) {
    this.animationDuration = (animationDuration < 500) ? 500 : animationDuration;
  }


  public Component animate(final Component toHide, final Component toShow, AnimationListener listener) {
    this.listener = listener;
    animationPanel = new SpecialPanel(this, (direction) ? toHide : toShow, (direction) ? toShow : toHide);
    animationPanel.needToStartThread = true;
    animationPanel.beginAngle = (direction) ? 0 : 180;
    animationPanel.endAngle = (direction) ? 180 : 0;
    animationPanel.setAnimationDuration(animationDuration);
    return animationPanel;


  }

  public Component getAnimationPanel() {
    return animationPanel;
  }

  void rotationFinished() {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        animationPanel = null;
        listener.animationFinished();
        listener = null;
      }
    });
  }


  class SpecialPanel extends JPanel {
    SlideAnimation owner;

    BufferedImage firstImage;
    BufferedImage secondImage;
    Component component1;
    Component component2;

    float angle = 0;

    public float beginAngle = 0;
    public float endAngle = 360;

    float deltaAngle = 0.1f;
    float effectTime = 2000;
    double dt = effectTime * deltaAngle / 180;
    int counter = 0;
    long totalDrawTime = 0;

    public boolean needToStartThread = false;

    SpecialPanel(SlideAnimation owner, BufferedImage firstImage, BufferedImage secondImage) {
      this.owner = owner;
      this.firstImage = firstImage;
      this.secondImage = secondImage;
      angle = beginAngle;
      setOpaque(false);
    }

    SpecialPanel(SlideAnimation owner, Component component1, Component component2) {
      this.owner = owner;
      this.component1 = component1;
      this.component2 = component2;
      angle = beginAngle;
      setOpaque(false);
    }

    public void setAnimationDuration(int animationDuration) {
      effectTime = (animationDuration < 500) ? 500 : animationDuration;
      dt = effectTime * deltaAngle / 180;
    }

    void startThread(float val1, float val2) {
      counter = 0;
      totalDrawTime = 0;
      this.beginAngle = val1;
      this.endAngle = val2;
      if (endAngle < beginAngle)
        deltaAngle = -Math.abs(deltaAngle);
      else
        deltaAngle = Math.abs(deltaAngle);
      angle = beginAngle;
      final Runnable repaint = new Runnable() { //am@kikamedical.com Arnaud Masson
        public void run() {
          repaint();
          getToolkit().sync();
        }
      };
      Thread t = new Thread(new Runnable() {
        public void run() {
          float absDeltaAngle = Math.abs(deltaAngle);
          long startTime = System.currentTimeMillis();
          long initTime = System.currentTimeMillis();
          while (true) {
            long time = System.currentTimeMillis();
            angle += deltaAngle * (time - startTime) / dt;//idea am@kikamedical.com Arnaud Masson
            startTime = time;
            if (((angle >= endAngle - deltaAngle / 2) && (deltaAngle > 0)) ||
                ((angle <= endAngle - deltaAngle / 2) && (deltaAngle < 0))) {
              angle = endAngle;
              if (Math.abs(angle - 360) < absDeltaAngle / 2) angle = 0;
              if (Math.abs(angle - 180) < absDeltaAngle / 2) angle = 180;
              repaint();
              //System.out.println("total count " + counter + " time " + (System.currentTimeMillis() - initTime) + " average time " + (totalDrawTime / counter));
              break;
            }
            if (angle >= 360) angle = 0;
            try {
              //Thread.sleep(dt);
              //repaint();
              //getToolkit().sync();
              SwingUtilities.invokeAndWait(repaint);  //idea am@kikamedical.com Arnaud Masson
            } catch (Throwable tt) {
            }
          }
          if (owner != null) owner.rotationFinished();
          synchronized (SpecialPanel.this) {
            if (component1 != null) firstImage = null;
            if (component2 != null) secondImage = null;
          }
        }
      });
      t.start();
    }

    public void update(Graphics g) {
      paint(g);
    }

    public synchronized void paint(Graphics g) {
      if (needToStartThread) {
        totalDrawTime = 0;
        counter = 0;
        needToStartThread = false;
        startThread(beginAngle, endAngle);
        if (firstImage == null) {
          firstImage = createImageFromComponent(component1);
        }
        if (secondImage == null) {
          secondImage = createImageFromComponent(component2);
        }
      }
      if (firstImage == null || secondImage == null) return;
      Graphics2D g2d = (Graphics2D) g;
      int ww = firstImage.getWidth();
      int hh = firstImage.getHeight();
      {
        BufferedImage currImage = null;
        int[] currPixels = null;
        int w = firstImage.getWidth();
        int offset = (int) (w * angle / 180);
        if (offset < 0) offset = 0;
        if (offset > w) offset = w;

        long beforeDraw = System.currentTimeMillis();
        g2d.drawImage(firstImage, null, 0, 0);
        g2d.drawImage(secondImage, null, w - offset, 0);
        totalDrawTime += (System.currentTimeMillis() - beforeDraw);
        counter++;

      }
    }

    BufferedImage createImageFromComponent(Component comp) {
      BufferedImage retImage = null;
      if (comp == null) return retImage;
      try {
        GraphicsEnvironment genv = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice gd = genv.getDefaultScreenDevice();
        GraphicsConfiguration gc = gd.getDefaultConfiguration();
        java.awt.image.ColorModel cm = gc.getColorModel();
        boolean hasAlpha = cm.hasAlpha();
        int cw = comp.getSize().width;
        int ch = comp.getSize().height;
        if (hasAlpha) {
          retImage = gc.createCompatibleImage(cw, ch);
        } else {
          retImage = new BufferedImage(cw, ch, BufferedImage.TYPE_INT_ARGB);
        }
        if (retImage == null) return retImage;
        Graphics og = retImage.getGraphics();
        comp.paint(og);
        og.dispose();
      } catch (Throwable t) {
      }
      return retImage;

    }

public BufferedImage
  createImageFromComponentWrong(
  Component c) {
  Dimension size = c.getSize();
  // Create a buffered image
  // equal to the size of the
  // component.
  BufferedImage bufferedImage =
    new BufferedImage(
    size.width, size.height,
    BufferedImage.TYPE_INT_RGB);
  // Get the graphics context of
  // the image
  Graphics bufferedGraphics =
    bufferedImage.
    createGraphics();
  // Request the component to
  // paint itself on to the
  // image's graphics. This
  // method would not work if
  // the component is not yet
  // visualized.
  c.paint(bufferedGraphics);
  return bufferedImage;
}

    BufferedImage createImageFromComponentMy(Component comp) {
      BufferedImage retImage = null;
      if (comp == null) return retImage;
      Image image = comp.createImage(comp.getSize().width, comp.getSize().height);
//      waitForImage(image, comp);
      retImage =
          new BufferedImage(image.getWidth(comp), image.getHeight(comp),
                            BufferedImage.TYPE_INT_RGB);
        Graphics2D g2d = retImage.createGraphics();
        g2d.drawImage(image, 0, 0, comp);
      return retImage;

    }

  }


}
