/*
 * @(#)MathOperation.java created 16/05/15 flight to Shanghai
 *
 * Copyright (c) 1996-2015 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of the author and it is 
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

import it.unitn.ing.rista.diffr.Parameter;
import net.objecthunter.exp4j.*;
import uk.co.cogitolearning.cogpar.*;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.*;

/**
 * The MathExpression is a class to implement mathematical operations
 * between parameters or values. The operation is identified by a
 * string and operate over two operands returning the result.
 * Result =
 *
 * @author luca
 * @version $Revision: 1.0 $, $Date: 16/05/15 21:40 $
 * @since JDK1.1
 */

//enum Operation {PLUS, MINUS, MULT, DIV, ELEV, EXP, LOG, COS, SIN, TAN, COT, PAR_LEFT, PAR_RIGHT, CONST}

public class MathExpression {

/*	static final String[] operationsString = {"+", "-", "*", "/", "^", "exp", "log",
			"cos", "sin", "tan", "cot", "(", ")"};*/

	Expression mathExpression = null;
	ExpressionNode nodeExpression = null; // alternative
	Parameter[] parameters;
	Parameter owner;
	String originalExpression = null;

	public MathExpression(Parameter parent, String expression, Parameter[] variables) {
		owner = parent;
		parameters = variables;
		originalExpression = expression;
		parseExpression();
	}

	public void parseExpression() {
		try {
			if (MaudPreferences.getBoolean("math_expression_parser.use_alternative", false)) {
				Vector<String> variables = new Vector<String>(parameters.length);
				for (int i = 0; i < parameters.length; i++)
					variables.add(parameters[i].toIDString());
				mathExpression = new ExpressionBuilder(originalExpression).variables().build();
			} else {
				nodeExpression = (new Parser()).parse(originalExpression);
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
	}

	public double evaluateExpression() {
		try {
			if (mathExpression != null) {
				for (int i = 0; i < parameters.length; i++)
					mathExpression = mathExpression.setVariable(parameters[i].refName, parameters[i].getValueD());
				return mathExpression.evaluate();
			} else {
				for (int i = 0; i < parameters.length; i++)
					nodeExpression.accept(new SetVariable(parameters[i].refName, parameters[i].getValueD()));
				return nodeExpression.getValue();
			}
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		return owner.getValueD(); // we do not change
	}

	public void writeExpression(BufferedWriter out) {
		try {
			out.write(" " + "#boundWithEquation {" + originalExpression + "}");
		} catch (IOException ioe) {
			System.out.println("Error in writing the parameter " + this);
		}
	}

	public void dispose() {
		if (mathExpression != null)
			mathExpression = null;
		if (nodeExpression != null)
			nodeExpression = null;
		for (int i = 0; i < parameters.length; i++) {
			parameters[i].unregister(this);
		}
		parameters = null;
	}

}
