package org.javadev.effects;

//import it.unitn.ing.rista.util.Misc;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.awt.image.*;


/**
 * The DashboardAnimation is a class to animate the transition between
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

public class DashboardAnimation implements Animation {

  SpecialPanel animationPanel = null;
  private AnimationListener listener = null;
  boolean direction = true;
  int       animationDuration = 2000;

  public void setDirection(boolean direction){
    this.direction = direction;
  }

  public void setAnimationDuration(int animationDuration){
    this.animationDuration = (animationDuration < 1000)?1000:animationDuration;
  }


  public Component animate(final Component toHide, final Component toShow, AnimationListener listener) {
    this.listener = listener;
    animationPanel = new SpecialPanel(this, (direction)?toHide:toShow, (direction)?toShow:toHide);
    animationPanel.beginAngle = (direction)?0:180;
    animationPanel.endAngle = (direction)?180:0;
    animationPanel.needToStartThread = true;
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

  class SpecialOp implements BufferedImageOp{
  double   angle = 0;
  double   epsilon = 0.3f;
  double   cosa = Math.cos(Math.PI*angle/180f);
  double   sinea = Math.sin(Math.PI*epsilon*angle/180f);
  boolean positiveDirection = false;
  int     []pixels = null;
  int     []destPixels = null;

  private Object kernelLock = new Object();
  BufferedImageOp convOp = null;
  BufferedImage dest1;


          SpecialOp(){
              this(0);
          }

          SpecialOp(double angle){
              super();
              setAngle(angle);
              setConvOp(1,20,1);
          }

          public void setAngle(double angle){
              this.angle = angle;
              cosa = Math.cos(Math.PI*angle/180f);
              sinea = Math.sin(Math.PI*epsilon*angle/180f);
          }

          public void setPositiveDirection(boolean positiveDirection){this.positiveDirection = positiveDirection;}

          public void setPixels(int []pixels){
              this.pixels = pixels;
              if(pixels == null){
                  destPixels = null;
              }else{
                  destPixels = new int[pixels.length];
              }
          }

          public synchronized BufferedImage filter(BufferedImage src, BufferedImage dest){
              if(dest == null && destPixels == null){
                  dest = createCompatibleDestImage(src,null);
              }
              int w = src.getWidth();
              int h = src.getHeight();
              double kw = w*(1-cosa);
              double k1 = sinea / (double)w;
              double  xx = kw;
              for(int x = 0; x < w; x++){
                  int k = (positiveDirection)?x:(w-x);
                  double k2 = 1 - k1 * k;
                  double k3 = (1 - k2) * h / 2;
                  int currIndex = x;
                  double  yy = k3;
                  for(int y = 0; y < h; y++){
                      int px = (pixels == null)?src.getRGB(x,y):pixels[currIndex];
                      int iyy = (int)yy;
                      int ixx = (int)xx;
                      if(ixx >= 0 && ixx < w && iyy >= 0 && iyy < h){
                          if(destPixels == null){
                              dest.setRGB(ixx,iyy,px);
                          }else{
                              destPixels[iyy*w+ixx] = px;
                          }
                      }
                      currIndex += w;
                      yy += k2;
                  }
                  xx += cosa;
              }
              if(destPixels != null && dest == null){
                  MemoryImageSource source = new MemoryImageSource(w, h, destPixels, 0, w);
                  Image img = Toolkit.getDefaultToolkit().createImage(source);
                  if(dest1 == null) dest1 = new BufferedImage(w,h,src.getType());
                  Graphics2D g2d = dest1.createGraphics();
        	          java.awt.Composite oldComp = g2d.getComposite();
        	          java.awt.Color oldColor = g2d.getColor();
                      g2d.setComposite(java.awt.AlphaComposite.getInstance(java.awt.AlphaComposite.CLEAR,0));
                      g2d.setColor(Color.white);
                      g2d.fillRect(0,0,w,h);//????
        	          g2d.setComposite(oldComp);
                      g2d.setColor(oldColor);
                      g2d.drawImage(img,0,0,null);
                  g2d.dispose();
                  if(false && convOp != null){//convolution slows down process try without it
                  try{
                    dest = convOp.filter(dest1, null);
                  }catch(Throwable t){
                  }
                  }else{
                      dest = dest1;
                  }
              }
              return dest;
          }
          public Rectangle2D getBounds2D (BufferedImage src){
              return src.getRaster().getBounds();
          }
          public BufferedImage createCompatibleDestImage (BufferedImage src,ColorModel destCM){
              if(destCM == null) destCM = src.getColorModel();
              int w = src.getWidth();
              int h = src.getHeight();
              return new BufferedImage(destCM,destCM.createCompatibleWritableRaster(w,h),destCM.isAlphaPremultiplied(),null);
          }
          public Point2D getPoint2D (Point2D srcPt, Point2D dstPt){
              if(dstPt == null) dstPt = new Point2D.Float();
              dstPt.setLocation(srcPt);
              return dstPt;
          }
          public RenderingHints getRenderingHints(){return null;}

          public void setConvOp(int dimensionOrder,float centralValue, float sideValue){
              float kernelCenter = centralValue;
              float kernelOuter = sideValue;
              int dimension = 2*dimensionOrder+1;
              int matrixSize = dimension*dimension;
              float   summ = (matrixSize - 1)*sideValue + centralValue;
              if(summ == 0){
                  kernelCenter = 1;
                  kernelOuter = 0;
                  summ = 1;
              }
              float []values = new float[matrixSize];
              int centralIndex = matrixSize / 2;
              for(int i = 0; i < matrixSize; i++){
                  values[i] = (i == centralIndex)?centralValue/summ:sideValue/summ;
              }

              synchronized(kernelLock){
                  Kernel kernel = new Kernel(dimension, dimension,values);
            if (kernel != null) convOp = new ConvolveOp(kernel);
              }
          }


  }

  class SpecialPanel extends JPanel{
  DashboardAnimation owner;
  BasicStroke stroke2 = new BasicStroke(2);
  BasicStroke stroke1 = new BasicStroke(1);

  BufferedImage firstImage;
  BufferedImage secondImage;
  Component component1;
  Component component2;

  int []firstPixels;
  int []secondPixels;

  SpecialOp op;
  double angle = 0;

  public double beginAngle = 0;
  public double endAngle = 360;

  double deltaAngle = 0.5f;
  double effectTime = 2000;
  double dt = effectTime* deltaAngle/180;
  int counter = 0;
  long totalDrawTime = 0;

  public boolean needToStartThread = false;
  private long startTime = 0;

      SpecialPanel(DashboardAnimation owner,BufferedImage firstImage, BufferedImage secondImage){
          this.owner = owner;
          this.firstImage = firstImage;
          this.secondImage = secondImage;
          angle = beginAngle;
          op = new SpecialOp(angle);
          setOpaque(false);
      }

      SpecialPanel(DashboardAnimation owner,Component component1, Component component2){
          this.owner = owner;
          this.component1 = component1;
          this.component2 = component2;
          angle = beginAngle;
          op = new SpecialOp(angle);
          setOpaque(false);
          firstImage = createImageFromComponent(component1);
          firstPixels = createPixels(firstImage);
          secondImage = createImageFromComponent(component2);
          secondPixels = createPixels(secondImage);
      }

      public void setAnimationDuration(int animationDuration){
        effectTime = (animationDuration < 1000)?1000:animationDuration;
        dt = effectTime* deltaAngle/180;
      }

      void startThread(double val1,double val2){
          counter = 0;
          totalDrawTime = 0;
          this.beginAngle = val1;
          this.endAngle = val2;
          if(endAngle < beginAngle)   deltaAngle = -Math.abs(deltaAngle);
          else                        deltaAngle = Math.abs(deltaAngle);
          angle = beginAngle;          
          final Runnable repaint = new Runnable() { //am@kikamedical.com Arnaud Masson
             public void run() {
                 repaint();
                 getToolkit().sync();
             }                                      };
          Thread t = new Thread(new Runnable(){
              public void run(){
                  double absDeltaAngle=Math.abs(deltaAngle);
                  long initTime = System.currentTimeMillis();
                  while(true){
                      if(((angle >= endAngle-deltaAngle/2) && (deltaAngle > 0)) ||
                         ((angle <= endAngle-deltaAngle/2) && (deltaAngle < 0))){
                          angle = endAngle;
                          if(Math.abs(angle - 360) < absDeltaAngle / 2) angle = 0;
                          if(Math.abs(angle - 180) < absDeltaAngle / 2) angle = 180;
                          repaint();
                          //System.out.println("total count "+counter+" time "+(System.currentTimeMillis() - initTime)+" average time "+(totalDrawTime/counter));
                          break;
                      }
                      if(angle >= 360) angle = 0;
                      if((int)angle % 90 == 0){
                          angle += deltaAngle;//skip "singularity"
                      }
                      try{
                          //Thread.sleep((long) dt);
                          //repaint();
                          //getToolkit().sync();
                         SwingUtilities.invokeAndWait(repaint);  //idea am@kikamedical.com Arnaud Masson
                      }catch(Throwable tt){
                      }
                  }
                  if(owner != null) owner.rotationFinished();
              }
          });
          t.start();
      }

      public void update(Graphics g){
          paint(g);
      }

      public synchronized void paint(Graphics g){
          if(needToStartThread){
              totalDrawTime = 0;
              counter = 0;
              needToStartThread = false;
              startThread(beginAngle,endAngle);
              angle = beginAngle;
              startTime = System.currentTimeMillis();
          }
          long time = System.currentTimeMillis();
          double oldAngle = angle;
          angle += deltaAngle*(time - startTime)/dt;//idea am@kikamedical.com Arnaud Masson
          if((angle >= endAngle && deltaAngle > 0) || (angle <= endAngle && deltaAngle < 0)) angle = endAngle;
          startTime = time;
          if(firstImage == null || secondImage == null) return;
          Graphics2D g2d = (Graphics2D)g;
          int ww = firstImage.getWidth();
          int hh = firstImage.getHeight();
          {
              BufferedImage currImage = null;
              int []currPixels = null;
              double needAngle = (180 - angle > 1 || angle > 1)?angle:oldAngle;
              boolean needDirection = false;
              if((angle >= 0 && angle < 90)/* || (angle >= 270 && angle < 360)*/){
                  currImage = firstImage;
                  currPixels = firstPixels;
              }else{
                  currImage = secondImage;
                  currPixels = secondPixels;
              }
              /*if(angle >= 270 && angle < 360){
                  needAngle = 360 - angle;
                  needDirection = true;
              }else if(angle >= 180 && angle < 270){
                  needAngle = angle - 180;
              }else */if(angle >= 90 && angle < 180){
                  needAngle = 180-angle;
                  needDirection = true;
              }

            if(currImage != null){
                  double cosa = Math.cos(Math.PI*needAngle/180);
                  op.setAngle(needAngle);
                  op.setPositiveDirection(needDirection);
                  op.setPixels(currPixels);
                  long beforeDraw = System.currentTimeMillis();
                  g2d.drawImage(currImage,op,-(int)Math.round((double)ww*(1-cosa)/2),0);
                  totalDrawTime += (System.currentTimeMillis() - beforeDraw);
                  counter++;
                  op.setPixels(null);
              }

        }
      }

      BufferedImage createImageFromComponent(Component comp){
          BufferedImage retImage = null;
          if(comp == null) return retImage;
          try{
              GraphicsEnvironment genv = GraphicsEnvironment.getLocalGraphicsEnvironment();
              GraphicsDevice gd = genv.getDefaultScreenDevice();
              GraphicsConfiguration gc = gd.getDefaultConfiguration();
              java.awt.image.ColorModel cm = gc.getColorModel();
            boolean hasAlpha = cm.hasAlpha();
            int cw = comp.getSize().width;
            int ch = comp.getSize().height;
            if(hasAlpha){
                retImage = gc.createCompatibleImage(cw,ch);
            }else{
                retImage = new BufferedImage(cw,ch,BufferedImage.TYPE_INT_ARGB);
            }
              if(retImage == null) return retImage;
              Graphics og = retImage.getGraphics();
                  comp.paint(og);
              og.dispose();
          }catch(Throwable t){
          }
          return retImage;

      }

      int [] createPixels(BufferedImage bim){
          int []pixels = null;
          if(bim == null) return pixels;
          int ww = bim.getWidth();
          int hh = bim.getHeight();
          pixels = new int[ww * hh];
          PixelGrabber pg = new PixelGrabber(bim, 0, 0, ww, hh, pixels, 0, ww);
          try {
              pg.grabPixels();
          } catch (InterruptedException e) {
              return null;
          }
          if ((pg.getStatus() & ImageObserver.ABORT) != 0) {
              return null;
          }
          return pixels;
      }

  }



}
