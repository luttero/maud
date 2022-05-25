package org.javadev.effects;

import java.awt.Component;

/**
 * @author Luca Lutterotti
 * @author Sam Berlin
 * @author Dmitry Markman
 */

public interface Animation {
    
    /**
     * Draws an animation moving from 'first' to 'last'.
     * AnimationListener listens to the animation and is notified
     * when the animation is finished.
     * @return a Component which the animation is drawn on.
     */
    public Component animate(Component first, Component last, AnimationListener listener);

    /**
     * set direction of the animation.
     * meaning of the param direction is the function of every particulatr animator
     * @param direction
     */
    public void setDirection(boolean direction);

    /**
     * set duration of the animation.
     * @param duration duration of the animation in the ms
     */
    public void setAnimationDuration(int duration);


}
