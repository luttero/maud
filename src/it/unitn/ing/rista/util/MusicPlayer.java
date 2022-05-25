/*
 * @(#)MusicPlayer.java created 12/05/2002 Casalino
 *
 * Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
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

import java.io.*;
import java.util.*;
import java.net.*;
import javax.sound.midi.*;
import javax.sound.sampled.*;

/**
 * The MusicPlayer is a class to play randomly a music file
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MusicPlayer implements Runnable, LineListener, MetaEventListener {

  Thread thread;
  Sequencer sequencer;
  boolean midiEOM;
  boolean audioEOM;
  MidiChannel[] channels;
  Synthesizer synthesizer;
  Object currentSound;
  String currentName;
  boolean bump;
  boolean paused = false;
  Vector sounds = null;

  public MusicPlayer(Vector soundsURLs) {
    sounds = soundsURLs;
  }

  public boolean loadSound(Object object) {
    if (object instanceof URL) {
      currentName = ((URL) object).getFile();
      try {
        currentSound = AudioSystem.getAudioInputStream((URL) object);
      } catch (Exception e) {
        try {
          currentSound = MidiSystem.getSequence((URL) object);
        } catch (InvalidMidiDataException imde) {
          System.out.println("Unsupported audio file.");
          return false;
        } catch (Exception ex) {
          ex.printStackTrace();
          currentSound = null;
          return false;
        }
      }
    } else if (object instanceof File) {
      currentName = ((File) object).getName();
      try {
        currentSound = AudioSystem.getAudioInputStream((File) object);
      } catch (Exception e1) {
        try {
          FileInputStream is = new FileInputStream((File) object);
          currentSound = new BufferedInputStream(is, 1024);
        } catch (Exception e3) {
          e3.printStackTrace();
          currentSound = null;
          return false;
        }
      }
    }
    if (sequencer == null) {
      System.out.println("Null sequencer");
      currentSound = null;
      return false;
    }
    if (currentSound instanceof AudioInputStream) {
      try {
        AudioInputStream stream = (AudioInputStream) currentSound;
        AudioFormat format = stream.getFormat();
        if (format.getEncoding() == AudioFormat.Encoding.ULAW
                || format.getEncoding() == AudioFormat.Encoding.ALAW) {
          AudioFormat tmp = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED,
                  format.getSampleRate(),
                  format.getSampleSizeInBits() * 2,
                  format.getChannels(),
                  format.getFrameSize() * 2,
                  format.getFrameRate(), true);
          stream = AudioSystem.getAudioInputStream(tmp, stream);
          format = tmp;
        }
        DataLine.Info info = new DataLine.Info(Clip.class, stream.getFormat(),
                ((int) stream.getFrameLength()
                * format.getFrameSize()));
        Clip clip = (Clip) AudioSystem.getLine(info);
        clip.addLineListener(this);
        clip.open(stream);
        currentSound = clip;
      } catch (Exception ex) {
        ex.printStackTrace();
        currentSound = null;
        return false;
      }
    } else if (currentSound instanceof Sequence
            || (currentSound instanceof BufferedInputStream)) {
      try {
        sequencer.open();
        if (currentSound instanceof Sequence)
          sequencer.setSequence((Sequence) currentSound);
        else
          sequencer.setSequence((BufferedInputStream) currentSound);
      } catch (InvalidMidiDataException imde) {
        System.out.println("Unsupported audio file.");
        currentSound = null;
        return false;
      } catch (Exception ex) {
        ex.printStackTrace();
        currentSound = null;
        return false;
      }
    }
    return true;
  }

  public void playSound() {
    setGain();
    setPan();
    midiEOM = audioEOM = bump = false;
    if (currentSound instanceof Sequence
            || (currentSound instanceof BufferedInputStream && thread != null)) {
//        System.out.println("Started");
      sequencer.start();
      while (!midiEOM && thread != null && !bump) {
        try {
          if (this != null) {
            /* empty */
          }
          Thread.sleep(99L);
        } catch (Exception e) {
          break;
        }
      }
//        System.out.println("Stop");
      sequencer.stop();
      sequencer.close();
    } else if (currentSound instanceof Clip && thread != null) {
//        System.out.println("Started as clip");
      Clip clip = (Clip) currentSound;
      clip.start();
      try {
        if (this != null) {
          /* empty */
        }
        Thread.sleep(99L);
      } catch (Exception exception) {
        /* empty */
      }
      while ((paused || clip.isActive()) && thread != null && !bump) {
        try {
          if (this != null) {
            /* empty */
          }
          Thread.sleep(99L);
        } catch (Exception e) {
          break;
        }
      }
//        System.out.println("Stop");
      clip.stop();
      clip.close();
    }
    currentSound = null;
  }

  public void open() {
    try {
      sequencer = MidiSystem.getSequencer();
      if (sequencer instanceof Synthesizer) {
        synthesizer = (Synthesizer) sequencer;
        channels = synthesizer.getChannels();
      }
    } catch (Exception ex) {
      ex.printStackTrace();
      return;
    }
    sequencer.addMetaEventListener(this);
  }

  public void close() {
    if (sequencer != null)
      sequencer.close();
  }

  public void setPan() {
    int value = 0; //panSlider.getValue();
    if (currentSound instanceof Clip) {
      try {
        Clip clip = (Clip) currentSound;
        FloatControl panControl = (FloatControl) clip.getControl(FloatControl.Type.PAN);
        panControl.setValue((float) value / 100.0F);
      } catch (Exception ex) {
        ex.printStackTrace();
      }
    } else if (currentSound instanceof Sequence || currentSound instanceof BufferedInputStream) {
      for (int i = 0; i < channels.length; i++)
        channels[i].controlChange(10, (int) (((double) value + 100.0) / 200.0 * 127.0));
    }
  }

  public void setGain() {
    double value = 0.8; //(double) gainSlider.getValue() / 100.0;
    if (currentSound instanceof Clip) {
      try {
        Clip clip = (Clip) currentSound;
        FloatControl gainControl = ((FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN));
        float dB = (float) (Math.log(value == 0.0 ? 1.0E-4 : value) / Math.log(10.0) * 20.0);
        gainControl.setValue(dB);
      } catch (Exception ex) {
        ex.printStackTrace();
      }
    } else if (currentSound instanceof Sequence || currentSound instanceof BufferedInputStream) {
      for (int i = 0; i < channels.length; i++)
        channels[i].controlChange(7, (int) (value * 127.0));
    }
  }

  public void update(LineEvent event) {
    if (event.getType() == LineEvent.Type.STOP && !paused)
      audioEOM = true;
  }

  public void meta(MetaMessage message) {
    if (message.getType() == 47)
      midiEOM = true;
  }

  public Thread getThread() {
    return thread;
  }

  public void start() {
    thread = new Thread(this);
    thread.setName("MusicPlayer");
    thread.start();
  }

  public void stop() {
    if (thread != null)
      thread.interrupt();
    thread = null;
  }

  public void run() {
    open();
    int num = (new Random(sounds.size())).nextInt(sounds.size());
//      System.out.println("Playing: " + sounds.get(num).toXRDcatString());
    if (loadSound(sounds.get(num)) == true)
      playSound();
    try {
      if (this != null) {
        /* empty */
      }
      Thread.sleep(222L);
    } catch (Exception e) {
      e.printStackTrace();
    }
    thread = null;
    currentName = null;
    currentSound = null;
    close();
  }

}

