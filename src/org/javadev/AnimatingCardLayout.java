package org.javadev;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import org.javadev.effects.*;


/**
 * A CardLayout that displays transitions between each card.
 *
 * The work is based on an idea spurred by an example shown on the java-dev list
 * on Apple's mailing lists by Dmitry Markman.  Luca Lutterotti
 * did an excellent first implementation of the idea.
 *
 * <p/>
 * Description, from the Sam Berlin words:
 * <p/>
 * JPanel panel = new JPanel();
 * AnimatingCardLayout layout = new AnimatingCardLayout();
 * layout.setAnimation(new MyAnimation()); // if someone wants to provide their own animation
 * panel.setLayout(layout);
 * panel.add(comp1, "page1");
 * panel.add(comp2, "page2");
 * <p/>
 * and then all someone has to do is call "layout.show(container, "page1");"
 * or layout.show(container, "page2");" and it would flip between the cards using the animation.
 *
 * @author Luca Lutterotti
 * @author Sam Berlin
 * @author Dmitry Markman
 */

public class AnimatingCardLayout extends CardLayout implements AnimationListener {
    
    /**
     * The animation that will be doing the transition effects.
     */
    private Animation animation = null;
    
    protected int animationDuration = 2000;

    /**
     * The Panel that is displaying the animations.
     */
    private Component animationPanel = null;
    
    /**
     * Whether or not an animation is currently active.
     */
    private boolean isAnimating = false;
    
    /**
     * The component to show after animation is done.
     */
    private Component toShow = null;
    
    /**
     * The list of component & names.  Necessary because 'vector' is package-private.
     */
    protected List comps = new ArrayList();
    
    // simple class to store both a name & component.  necessary because
    // 'Card' is package private.
    protected static class Tuple {
        String name;
        Component comp;
        
        public Tuple(String name, Component comp) {
            this.name = name;
            this.comp = comp;
        }
        
        public boolean equals(Object o) {
            if(o == this)
                return true;
            if(!(o instanceof Tuple))
                return false;
            return name.equals(((Tuple)o).name);
        }
    }
    
    
    /**
     * Simple constructor.  To add an animation, use setAnimation(Animation).
     */
    public AnimatingCardLayout() {
        super();
    }
    
    /**
     * Constructor that takes an animation.
     */
    public AnimatingCardLayout(Animation anim) {
        animation = anim;
    }
    
    /**
     * Adds the given component to this layout with the given name.
     */
    public void addLayoutComponent(String name, Component comp) {
        Tuple tuple = new Tuple(name, comp);
        int idx = comps.indexOf(tuple);
        // if doesn't exist, add new.
        if(idx == -1)
            comps.add(tuple);
        else {
            // if exists, replace comp.
            Tuple existing = (Tuple)comps.get(idx);
            existing.comp = comp;
        }

        super.addLayoutComponent(name, comp);
    }
    
    /**
     * Removes the given component from this layout.
     */
    public void removeLayoutComponent(Component comp) {
        for(int i = 0; i < comps.size(); i++) {
            Tuple next = (Tuple)comps.get(i);
            if(next.comp == comp)
                comps.remove(i);
        }
        super.removeLayoutComponent(comp);
    }

    /**
    * Shows an animation while flipping to the first card.
    */
    public void first(Container parent) {
        synchronized(parent.getTreeLock()) {
            animate(parent, parent.getComponent(0),false);
        }
    }

    /**
     * Shows an animation while flipping to the next card.
     */
    public void next(Container parent) {
        synchronized (parent.getTreeLock()) {
            int ncomponents = parent.getComponentCount();
            if (ncomponents > 0) {
              int nextCard = (getCurrent(parent) + 1) % ncomponents;
              animate(parent, parent.getComponent(nextCard),true);
            }
        }
    }

    /**
    * Shows an animation while flipping to the previous card.
    */
    public void previous(Container parent) {
        synchronized (parent.getTreeLock()) {
            int ncomponents = parent.getComponentCount();
            int currentCard = getCurrent(parent);
            int nextCard = currentCard > 0 ? currentCard - 1 : ncomponents - 1;
            animate(parent, parent.getComponent(nextCard),false);
        }
    }

    /**
     * Shows an animation while flipping to the last card.
     */
    public void last(Container parent) {
        synchronized (parent.getTreeLock()) {
            int ncomponents = parent.getComponentCount();
            animate(parent, parent.getComponent(ncomponents-1),true);
        }
    }

    /**
     * Flips to the specified card, using an animation.
     */
    public void show(Container parent, String name) {
        synchronized (parent.getTreeLock()) {
            int idx = indexOf(name);
            if(idx != -1)
                animate(parent, parent.getComponent(idx),(idx >= getCurrent(parent)));
        }
    }
    
    /**
     * Notification that animation has finished.
     */
    public void animationFinished() {
        Container parent = animationPanel.getParent();
        Component next = toShow;
        isAnimating = false;
        toShow = null;
        parent.remove(animationPanel);
        animationPanel = null;
        super.show(parent, nameOf(next));
    }

    /**
     * Animates moving from the current component
     * to the given component, in the given container.
     */
    private void animate(Container parent, Component toGoTo, boolean direction) {
        if(isAnimating)
            throw new IllegalStateException("already animating");
            
        Component current = current(parent);
        // ignore animation attempt from the animation panel itself --
        // this happens when we remove it, since it's the currently
        // visible panel.
        if(current == animationPanel)
            return;
        
        String nextName = nameOf(toGoTo);
        String curName = nameOf(current);
        
        if(animation == null || current == toGoTo || current == null || curName == null) {
            super.show(parent, nextName);
        } else {
            isAnimating = true;
            toShow = toGoTo;
            animation.setDirection(direction);
            animation.setAnimationDuration(animationDuration);
            animationPanel = animation.animate(current, toShow, this);
            parent.add(animationPanel, "__animation__");
            animationPanel.setVisible(true);
            current.setVisible(false);
            parent.validate();
       }
    }
    
    /**
     * Returns the index of the specified component's name, as it was
     * added to this layout manager.
     */
    private int indexOf(String name) {
        for(int i = 0; i < comps.size(); i++) {
            Tuple next = (Tuple)comps.get(i);
            if(next.name.equals(name))
                return i;
        }
        return -1;
    }
    
    /**
     * Returns the name for the given component.
     */
    private String nameOf(Component comp) {
        for(int i = 0; i < comps.size(); i++) {
            Tuple next = (Tuple)comps.get(i);
            if(next.comp == comp)
                return next.name;
        }
        return null;
    }        
    
    /**
     * Returns the currently visible component.
     */
    private Component current(Container parent) {
        int size = parent.getComponentCount();
        for(int i = 0; i < size; i++) {
            Component next = parent.getComponent(i);
            if(next.isVisible())
                return next;
        }
        return null;
    }
    
    /**
     * Returns the currently visible component in the given container.
     */
    private int getCurrent(Container parent) {
        int size = parent.getComponentCount();
        for(int i = 0; i < size; i++)
            if(parent.getComponent(i).isVisible())
                return i;
        return -1;
    }
    
    /**
     * Gets the current Animation.
     */
    public Animation getAnimation() {
        return animation;
    }

    /**
     * Sets the current Animation.
     */
    public void setAnimation(Animation animation) {
        this.animation = animation;
    }
    
    /**
     * set duration of the animation.
     * @param duration duration of the animation in the ms
     */
    public void setAnimationDuration(int animationDuration){
        this.animationDuration = (animationDuration < 500)?500:animationDuration;
    }
}
