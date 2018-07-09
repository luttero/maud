package com.objectwave.viewUtility;
import java.awt.event.*;

import javax.swing.*;

/**
 * This class with call it's onChange() method whenever the given
 * text field's value changes.  This is useful for triggering other
 * actions (populating other controls based on the change, for instance),
 * or for executing validations.
 *
 * <p>This class is intended to be initialized a little differently than is
 * normally done, as it is self-registering.  Instead of passing an instance
 * of this class to a JTextField's addXxxListener() method, create an
 * instance of this listener and pass the JTextField in the constructor.  The
 * constructor will register whatever is necessary.
 *
 * <p>There is no need to maintain a local reference to an instance of this
 * listener since the JTextField will refer to it, thereby preventing garbage
 * collection (at least until the JTextField is collected).
 *
 * <p>Example:
 * <pre>
 *   // ...
 *   JTextField tf = new JTextField();
 *   // ...
 *   new TextFieldChangeListener(tf)
 *     {
 *       public abtract void onChange(String oldText, String newText)
 *       {
 *         System.out.println("Changed from '" + oldText + "' to '" + newText + "'");
 *       }
 *     };
 *   // ...
 * </pre>
 *
 * @author  Steven Sinclair
 * @version  1.0
 */
public abstract class TextFieldChangeListener implements FocusListener, ActionListener
{
	protected JTextField textField;

	/**
	 * Construct an instance of this class with the given text field.
	 *
	 * @param  newTextField
	 */
	public TextFieldChangeListener(JTextField newTextField)
	{
		setTextField(newTextField);
	}

	/**
	 * Set the text field and register self as focus and action listener.
	 *
	 * @param  newTextField The new TextField value
	 */
	protected void setTextField(JTextField newTextField)
	{
		if(newTextField != textField)
		{
			this.textField = newTextField;
			getTextField().addFocusListener(this);
			getTextField().addActionListener(this);
		}
	}

	/**
	 * @return  JTextField - the text field associated with this listener
	 */
	public JTextField getTextField()
	{
		return textField;
	}

	/**
	 * When the control gains focus, 'remember' the current field text.
	 * Called by the control as part of the FocusListener interface.
	 *
	 * @param  e
	 */
	public void focusGained(FocusEvent e)
	{
		onChange(getTextField().getText());
	}

	/**
	 * When the control loses focus, and the event has not been handled,
	 * call the onChange method.
	 * Called by the control as part of the FocusListener interface.
	 *
	 * @param  e
	 */
	public void focusLost(FocusEvent e)
	{
		onChange(getTextField().getText());
	}

	/**
	 * When the control performs and action, and the event has not been handled,
	 * call the onChange method.
	 *
	 * @param  e
	 */
	public void actionPerformed(ActionEvent e)
	{
		onChange(getTextField().getText());
	}

	/**
	 * Abstract method which must be implemented to define what action should be
	 * taken when the field focus changes.
	 *
	 * @param  newText
	 */
	public abstract void onChange(String newText);
}
