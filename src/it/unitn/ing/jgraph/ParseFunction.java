package it.unitn.ing.jgraph;


import java.lang.*;

/*
**************************************************************************
**
**    Class  ParseFunction
**
**************************************************************************
**    Copyright (C) 1996 Leigh Brookshaw
**
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
**    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**************************************************************************/

/**
 * This class will parse a function definition and solve it returning the
 * value. The function may have upto 3 independent variables in it
 * (x,y,z).
 *
 * <p><b>Known Bugs</b> This class is not fool proof. If the answer is wrong
 * then use the parenthesis to force the order of evaluation. The most
 * likely place this will be needed is in the use of the power command.
 * The exponent is not evaluated correctly if it begins with a
 * unary operator.
 *
 * <h3>List of recognised commands</h3>
 * <ul>
 *   <li> ( ) parenthesis , comma
 *   <li> +, -, unary -, unary +
 *   <li> *, /
 *   <li> ^ (raise to a power)
 *   <li> pi, e, All the constants in class SpecialFunction
 *   <li> log
 *   <li> sin, cos, tan
 *   <li> asin, acos, atan
 *   <li> sqrt
 *   <li> rand
 *   <li> exp
 *   <li> remainder
 *   <li> atan2
 *   <li> All the functions in class SpecialFunction
 *   <li> Independent variables x,y,z
 *   <li> Scientific notation using "e", "E", "d", "D".
 * </ul>
 *
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:51 $
 * @author Leigh Brookshaw
 */

public class ParseFunction extends ScanString {

/*
**********************
**  Constants
*********************/

/*
**   Specifiy the integer values associated with keywords. Every integer
**   in this list is associated with keyword used in the string
*/
  static final int GROUP = 1;
  static final int ENDGROUP = 2;
  static final int ADD = 3;
  static final int SUBTRACT = 4;
  static final int DIVIDE = 5;
  static final int MULTIPLY = 6;
  static final int LOG = 7;
  static final int POWER = 8;
  static final int PI = 9;
  static final int E = 10;
  static final int SIN = 11;
  static final int COS = 12;
  static final int TAN = 13;
  static final int X = 14;
  static final int Y = 15;
  static final int Z = 16;
  static final int ASIN = 17;
  static final int ACOS = 18;
  static final int ATAN = 19;
  static final int RAD = 20;
  static final int SQRT = 21;
  static final int RANDOM = 22;
  static final int LOG10 = 23;
  static final int EXP = 24;
  static final int REMAINDER = 25;
  static final int COMMA = 26;
  static final int ATAN2 = 27;
  static final int J0 = 28;
  static final int J1 = 29;
  static final int JN = 30;
  static final int SINH = 31;
  static final int COSH = 32;
  static final int TANH = 33;
  static final int ASINH = 34;
  static final int ACOSH = 35;
  static final int ATANH = 36;
  static final int Y0 = 37;
  static final int Y1 = 38;
  static final int YN = 39;
  static final int FAC = 40;
  static final int GAMMA = 41;
  static final int ERF = 42;
  static final int ERFC = 43;
  static final int NORMAL = 44;
  static final int POISSONC = 45;
  static final int POISSON = 46;
  static final int CHISQC = 47;
  static final int CHISQ = 48;
  static final int IGAM = 49;
  static final int IGAMC = 50;

  /*
  **   Physical Constants in SI units
  */
  static final int BOLTZMAN = 101;
  static final int ECHARGE = 102;
  static final int EMASS = 103;
  static final int PMASS = 104;
  static final int GRAV = 105;
  static final int PLANCK = 106;
  static final int LIGHTSPEED = 107;
  static final int STEFANBOLTZ = 108;
  static final int AVOGADRO = 109;
  static final int GASCONSTANT = 110;
  static final int GRAVACC = 111;






/*
*********************
**
** Private Variables
**
*********************/

  /*
  ** The root node after parsing the string
  */
  private Node root;
  /*
  ** Boolean flags set when any of the independent variable are encountered
  */
  private boolean independent_x;
  private boolean independent_y;
  private boolean independent_z;
  /*
  ** Internal values for the independent variables
  */
  private double x;
  private double y;
  private double z;
  /**
   * Debug variable. If set true debug output is printed.
   */
  public boolean debug;



/*
************************
**
** Constructors
**
**********************/


  /**
   * Instantiate the class
   */
  public ParseFunction() {

    root = null;
    independent_x = false;
    independent_y = false;
    independent_z = false;

    debug = false;

    x = 0.0;
    y = 0.0;
    z = 0.0;


    addKeyWord(",", COMMA);
    addKeyWord("(", GROUP);
    addKeyWord(")", ENDGROUP);
    addKeyWord("+", ADD);
    addKeyWord("-", SUBTRACT);
    addKeyWord("/", DIVIDE);
    addKeyWord("*", MULTIPLY);
    addKeyWord("log", LOG);
    addKeyWord("^", POWER);
    addKeyWord("pi", PI);
    addKeyWord("e", E);
    addKeyWord("sin", SIN);
    addKeyWord("cos", COS);
    addKeyWord("tan", TAN);
    addKeyWord("x", X);
    addKeyWord("y", Y);
    addKeyWord("z", Z);
    addKeyWord("asin", ASIN);
    addKeyWord("acos", ACOS);
    addKeyWord("atan", ATAN);
    addKeyWord("rad", RAD);
    addKeyWord("sqrt", SQRT);
    addKeyWord("rand", RANDOM);
    addKeyWord("log10", LOG10);
    addKeyWord("exp", EXP);
    addKeyWord("rem", REMAINDER);
    addKeyWord("atan2", ATAN2);
    addKeyWord("j0", J0);
    addKeyWord("j1", J1);
    addKeyWord("jn", JN);
    addKeyWord("sinh", SINH);
    addKeyWord("cosh", COSH);
    addKeyWord("tanh", TANH);
    addKeyWord("asinh", ASINH);
    addKeyWord("acosh", ACOSH);
    addKeyWord("atanh", ATANH);
    addKeyWord("y0", Y0);
    addKeyWord("y1", Y1);
    addKeyWord("yn", YN);
    addKeyWord("fac", FAC);
    addKeyWord("gamma", GAMMA);
    addKeyWord("erf", ERF);
    addKeyWord("erfc", ERFC);
    addKeyWord("normal", NORMAL);
    addKeyWord("poissonc", POISSONC);
    addKeyWord("poisson", POISSON);
    addKeyWord("chisq", CHISQ);
    addKeyWord("chisqc", CHISQC);
    addKeyWord("igam", IGAM);
    addKeyWord("igamc", IGAMC);


    addKeyWord("k", BOLTZMAN);
    addKeyWord("ec", ECHARGE);
    addKeyWord("me", EMASS);
    addKeyWord("mp", PMASS);
    addKeyWord("gc", GRAV);
    addKeyWord("h", PLANCK);
    addKeyWord("c", LIGHTSPEED);
    addKeyWord("sigma", STEFANBOLTZ);
    addKeyWord("na", AVOGADRO);
    addKeyWord("r", GASCONSTANT);
    addKeyWord("g", GRAVACC);


  }

  /**
   * Instantiate the class and define the string to parse.
   * @param s The string to be parsed.
   */
  public ParseFunction(String s) {
    this();
    setString(s);
  }

/*
******************
**
** Public Methods
**
*****************/

  /**
   * Parse the string.
   * @param s The string to parse
   * @return true if it was successful, false otherwise.
   */
  public boolean parse(String s) {
    setString(s);
    return parse();
  }


  /**
   * Parse the previously set string
   * @return true if it was successful, false otherwise.
   */

  public boolean parse() {
    root = new Node();
    if (parseString(root) != EOS) return false;
    if (debug) {
      Graph2D.out.println("Before Reordering:");
      root.print(5);
    }
    reOrderNodes(root);
    if (debug) {
      Graph2D.out.println("After Reordering:");
      root.print(5);
    }
    return true;
  }

  /**
   * Return the solution of the function given the independent values
   * @param x indpendent x value
   * @param y indpendent y value
   * @param z indpendent z value
   * @return solution of the function
   */
  public double getResult(double x, double y, double z)
          throws Exception {

    this.x = x;
    this.y = y;
    this.z = z;

    return evaluate(root);
  }

  /**
   * Return the solution of the function given the independent values
   * @param x indpendent x value
   * @param y indpendent y value
   * @return solution of the function
   */
  public double getResult(double x, double y)
          throws Exception {

    this.x = x;
    this.y = y;

    return evaluate(root);
  }

  /**
   * Return the solution of the function given the independent values
   * @param x indpendent x value
   * @return solution of the function
   */
  public double getResult(double x) throws Exception {

    this.x = x;

    return evaluate(root);
  }

  /**
   * Return the solution of the function if it has no independent values
   * or they have already been set using the set methods
   * @return solution of the function
   * @see this.setX()
   * @see this.setY()
   * @see this.setZ()
   */
  public double getResult() throws Exception {

    return evaluate(root);
  }


  /**
   * Return an array of solutions given an array of x values
   * @param n number of values to process in the input array
   * @param x Array containing the x values.
   * @return Array containing the solutions.
   * @exception Exception
   *             Generic exception if the array index n<=0, or x is null.
   */
  public double[] getResults(int n, double x[]) throws Exception {

    if (n <= 0) throw new Exception("Array index error");
    if (x == null) throw new Exception("X Array error");

    double array[] = new double[n];

    for (int i = 0; i < n; i++) {
      this.x = x[i];
      array[i] = evaluate(root);
    }
    return array;
  }

  /**
   * Return an array of solutions given an array of x values and y values
   * @param n number of values to process in the input array
   * @param x Array containing the x values.
   * @param y Array containing the y values.
   * @return Array containing the solutions.
   * @exception Exception
   *       Generic exception if the array index n<=0, or x is null, or y is null.
   */
  public double[] getResults(int n, double x[], double y[])
          throws Exception {

    if (n <= 0) throw new Exception("Array index error");
    if (x == null) throw new Exception("X Array error");
    if (y == null) throw new Exception("Y Array error");

    double array[] = new double[n];

    for (int i = 0; i < n; i++) {
      this.x = x[i];
      this.y = y[i];
      array[i] = evaluate(root);
    }
    return array;
  }

  /**
   * Return an array of solutions given an array of x values, y values
   * and z values.
   * @param n number of values to process in the input array
   * @param x Array containing the x values.
   * @param y Array containing the y values.
   * @return Array containing the solutions.
   * @exception Exception
   *      Generic exception if the array index n<=0, or x is null,
   *    or y is null, or z is null.
   */
  public double[] getResults(int n, double x[], double y[], double z[])
          throws Exception {

    if (n <= 0) throw new Exception("Array index error");
    if (x == null) throw new Exception("X Array error");
    if (y == null) throw new Exception("Y Array error");
    if (z == null) throw new Exception("Z Array error");

    double array[] = new double[n];

    for (int i = 0; i < n; i++) {
      this.x = x[i];
      this.y = y[i];
      this.z = z[i];
      array[i] = evaluate(root);
    }
    return array;
  }

  /**
   * Return a boolean array with index 0 true if the independent
   * variable x was found in the function, index 1 true if
   * y was found, and index 2  true if z was found.
   */

  public boolean[] getVariables() {
    boolean b[] = new boolean[3];

    b[0] = independent_x;
    b[1] = independent_y;
    b[2] = independent_z;

    return b;
  }

  /**
   * Set the value of the independent variable X.
   */
  public void setX(double x) {
    this.x = x;
  }

  /**
   * Set the value of the independent variable Y.
   */
  public void setY(double y) {
    this.y = y;
  }

  /**
   * Set the value of the independent variable Z.
   */
  public void setZ(double z) {
    this.z = z;
  }


/*
*******************
**
** Private Methods
**
******************/

  /*
  ** Parse the string and build the node link list.
  ** This builds up a simple Left-Right binary node tree.
  **
  ** A GROUP token (start of parenthesis) forces a new group node
  ** that starts an independent branch of the tree with the contents
  ** of the group linked to the left node of the group node.
  ** Intrinsic functions behave like a GROUP token they start an independent
  ** branch of the tree with the contents of the group linked to the
  ** left node of the intrinsic function.
  ** Intrinsic functions that have to be passed 2 parameters have the
  ** second parameter linked to there right node.
  **  (This has to be modified for functions containing more than 2 parameters
  **  currently the code cannot deal with more than 2 parameters)
  */
  private int parseString(Node node) {
    Node left;
    Node right;
    int token;
    int t;

    // Get the next token in the string
    token = nextWord();

    // Do some preliminary branching.
    if (token == ERROR) {
      Graph2D.out.println("Error parsing \"" + sval + "\"");
      return ERROR;
    } else if (token != EOS && debug) {
      Graph2D.out.println("Parse: " + sval + "\t Token: " + token);
    } else if (token == EOS && debug) {
      Graph2D.out.println("Parse: EOS");
    }

    // Main token switch

    switch (token) {

/*
	  ** Number token or constant tokens.
          ** Place the value in the node and recurse on this
          ** terminal node. It will be used by an operator.
          */
      case NUMBER:
        node.type = Node.VALUE;
        node.value = nval;
        return parseString(node);

      case PI:
        node.type = Node.VALUE;
        node.value = Math.PI;
        return parseString(node);

      case E:
        node.type = Node.VALUE;
        node.value = Math.E;
        return parseString(node);
      case BOLTZMAN:
        node.type = Node.VALUE;
        node.value = SpecialFunction.BOLTZMAN;
        return parseString(node);
      case ECHARGE:
        node.type = Node.VALUE;
        node.value = SpecialFunction.ECHARGE;
        return parseString(node);
      case EMASS:
        node.type = Node.VALUE;
        node.value = SpecialFunction.EMASS;
        return parseString(node);
      case PMASS:
        node.type = Node.VALUE;
        node.value = SpecialFunction.PMASS;
        return parseString(node);
      case GRAV:
        node.type = Node.VALUE;
        node.value = SpecialFunction.GRAV;
        return parseString(node);
      case PLANCK:
        node.type = Node.VALUE;
        node.value = SpecialFunction.PLANCK;
        return parseString(node);
      case LIGHTSPEED:
        node.type = Node.VALUE;
        node.value = SpecialFunction.LIGHTSPEED;
        return parseString(node);
      case STEFANBOLTZ:
        node.type = Node.VALUE;
        node.value = SpecialFunction.STEFANBOLTZ;
        return parseString(node);
      case AVOGADRO:
        node.type = Node.VALUE;
        node.value = SpecialFunction.AVOGADRO;
        return parseString(node);
      case GASCONSTANT:
        node.type = Node.VALUE;
        node.value = SpecialFunction.GASCONSTANT;
        return parseString(node);
      case GRAVACC:
        node.type = Node.VALUE;
        node.value = SpecialFunction.GRAVACC;
        return parseString(node);
      case RAD:
        node.type = Node.VALUE;
        node.value = Math.PI / 180.0;
        return parseString(node);

      case RANDOM:
        node.type = Node.VALUE;
        node.value = Math.random();
        return parseString(node);

/*
	 ** Independent variables behave like constant nodes except
         ** they are flagged as INDEPENDENT nodes. Then we recurse using this
         **  this node as it has to be used by an operator
         */
      case X:
      case Y:
      case Z:
        node.op = token;
        node.type = Node.INDEPENDENT;


        if (token == X)
          independent_x = true;
        else if (token == Y)
          independent_y = true;
        else if (token == Z) independent_z = true;

        return parseString(node);

/*
	** Terminal tokens
        */
      case ENDGROUP:
      case EOS:
      case COMMA:
        break;

        /*
        ** beginning of a group '('. Parse the string until the
        ** corresponding endgroup is encountered ')'. The created
        ** node list is attached to the group nodes left
        ** node. Then continue the process by continuing to parse
        ** the string after the endgroup.
        */
      case GROUP:
        left = new Node();
        if (parseString(left) == ENDGROUP) {
          node.left = left;
          node.type = Node.GROUP;
          node.precedence = Node.P5;
          token = parseString(node);
        } else {
          Graph2D.out.println("Parse Failed: missing parentheses");
          token = ERROR;
        }
        break;

/*
	** Binary and Unary Operators.
        ** The existing node goes to the left and everything
        ** on the right gets attached to the right node.
        **
        ** A unary operator is recognised by the empty
        ** node parsed ie nothing exists on the left.
        */
      case ADD:
      case SUBTRACT:
      case MULTIPLY:
      case DIVIDE:
      case POWER:
        right = new Node();
        t = parseString(right);


        if (t != ERROR) {
          if ((token == SUBTRACT || token == ADD)
                  && node.type == Node.NULL) {

            //System.out.println("...Unary Operator");

            node.right = right;
            node.precedence = Node.P4;
            node.op = token;
            node.type = Node.OP;
          } else {

            //System.out.println("...Binary Operator");

            left = new Node(node);
            node.left = left;
            node.right = right;
            node.op = token;
            node.type = Node.OP;
            switch (token) {
              case ADD:
              case SUBTRACT:
                node.precedence = Node.P1;
                break;
              case MULTIPLY:
              case DIVIDE:
                node.precedence = Node.P2;
                break;
              case POWER:
                node.precedence = Node.P3;
                break;
            }
          }
        }
        token = t;

        break;
/*
	** Single parameter intrinsic functions behave excacty like
        ** parenthesis.
        */
      case SIN:
      case COS:
      case TAN:
      case ASIN:
      case ACOS:
      case ATAN:
      case LOG:
      case SQRT:
      case LOG10:
      case EXP:
      case REMAINDER:
      case J0:
      case J1:
      case Y0:
      case Y1:
      case SINH:
      case COSH:
      case TANH:
      case ASINH:
      case ACOSH:
      case ATANH:
      case FAC:
      case GAMMA:
      case ERF:
      case ERFC:
      case NORMAL:
        node.op = token;
        node.type = Node.INTRINSIC;
        node.precedence = Node.P0;

        token = nextWord();
        if (token != GROUP) {
          Graph2D.out.println(
                  "Parse Failed: intrinsic function is missing \"(\"");
          token = ERROR;
        } else {
          left = new Node();
          if (parseString(left) == ENDGROUP) {
            node.left = left;
            token = parseString(node);
          } else {
            Graph2D.out.println(
                    "Parse Failed: intrinsic function is missing \")\"");
            token = ERROR;
          }
        }
        break;
/*
	** 2 parameter intrinsic functions
        ** First parameter on attached to the left node,
        **  second parameter attached to the right.
        */
      case ATAN2:
      case JN:
      case YN:
      case IGAM:
      case IGAMC:
      case CHISQ:
      case CHISQC:
      case POISSON:
      case POISSONC:
        node.op = token;
        node.type = Node.INTRINSIC;
        node.precedence = Node.P0;

        token = nextWord();
        if (debug) Graph2D.out.println("Parse: " + sval);
        if (token != GROUP) {
          Graph2D.out.println(
                  "Parse Failed: intrinsic function is missing \"(\"");
          token = ERROR;
        } else {
          Node param1 = new Node();
          if (parseString(param1) == COMMA) {
            Node param2 = new Node();
            if (parseString(param2) == ENDGROUP) {
              node.right = param2;
              node.left = param1;
              token = parseString(node);
            } else {
              Graph2D.out.println(
                      "Parse Failed: intrinsic function is missing \")\"");
              token = ERROR;
            }
          } else {
            Graph2D.out.println(
                    "Parse Failed: intrinsic function is missing \",\"");
            token = ERROR;
          }
        }
        break;


      default:
        break;
    }


    return token;


  }


  /*
  ** Starting at the passed node evaluate everything below this node
  */
  private double evaluate(Node node) throws Exception {
    double value = 0.0;


    if (node == null) {
      throw new
              Exception("evaluate: Failed because of null node!");
    }

    switch (node.type) {

      case Node.GROUP:
        value = evaluate(node.left);
        break;
      case Node.OP:
        value = evaluateOp(node);
        break;

      case Node.INTRINSIC:
        value = evaluateIntrinsic(node);
        break;
      case Node.VALUE:
        value = node.value;
        break;
      case Node.INDEPENDENT:
        if (node.op == X)
          value = x;
        else if (node.op == Y)
          value = y;
        else if (node.op == Z) value = z;
        break;
      default:
        throw new Exception("evaluate: Unknown type!");

    }

    return value;


  }

  /*
  ** Parsed node is an Operator node. Evaluate both the left and right
  ** nodes and then excecute the operator on the values.
  */
  private double evaluateOp(Node node) throws Exception {
    double value = 0.0;


    switch (node.op) {

      case ADD:
        if (node.left != null) value = evaluate(node.left);
        value += evaluate(node.right);
        break;
      case SUBTRACT:
        if (node.left != null) value = evaluate(node.left);
        value -= evaluate(node.right);
        break;
      case DIVIDE:
        value = evaluate(node.left);
        value /= evaluate(node.right);
        break;
      case MULTIPLY:
        value = evaluate(node.left);
        value *= evaluate(node.right);
        break;
      case POWER:
        value = Math.pow(evaluate(node.left),
                evaluate(node.right));
        break;
      default:
        throw new Exception(
                "evaluate: Failed because of Unknown operator!");
    }

    return value;

  }

  /*
  ** Parsed node is an instrinsic function. Evaluate the parameters
  ** then call the intrinsic and return the result
  */
  private double evaluateIntrinsic(Node node) throws Exception {
    double value = 0.0;


    switch (node.op) {

      case SIN:
        value = Math.sin(evaluate(node.left));
        break;
      case COS:
        value = Math.cos(evaluate(node.left));
        break;
      case TAN:
        value = Math.tan(evaluate(node.left));
        break;
      case ASIN:
        value = Math.asin(evaluate(node.left));
        break;
      case ACOS:
        value = Math.acos(evaluate(node.left));
        break;
      case ATAN:
        value = Math.atan(evaluate(node.left));
        break;
      case LOG:
        value = Math.log(evaluate(node.left));
        break;
      case SQRT:
        value = Math.sqrt(evaluate(node.left));
        break;

      case LOG10:
        value = Math.log(evaluate(node.left)) /
                Math.E;
      case EXP:
        value = Math.exp(evaluate(node.left));
        break;
      case J0:
        value = SpecialFunction.j0(
                evaluate(node.left));
        break;
      case J1:
        value = SpecialFunction.j1(
                evaluate(node.left));
        break;
      case Y0:
        value = SpecialFunction.y0(
                evaluate(node.left));
        break;
      case Y1:
        value = SpecialFunction.y1(
                evaluate(node.left));
        break;
      case FAC:
        value = SpecialFunction.fac(
                evaluate(node.left));
        break;
      case GAMMA:
        value = SpecialFunction.gamma(
                evaluate(node.left));
        break;
      case SINH:
        value = SpecialFunction.sinh(
                evaluate(node.left));
        break;
      case COSH:
        value = SpecialFunction.cosh(
                evaluate(node.left));
        break;
      case TANH:
        value = SpecialFunction.tanh(
                evaluate(node.left));
        break;
      case ASINH:
        value = SpecialFunction.asinh(
                evaluate(node.left));
        break;
      case ACOSH:
        value = SpecialFunction.acosh(
                evaluate(node.left));
        break;
      case ATANH:
        value = SpecialFunction.atanh(
                evaluate(node.left));
        break;

      case ERF:
        value = SpecialFunction.erf(
                evaluate(node.left));
        break;

      case ERFC:
        value = SpecialFunction.erfc(
                evaluate(node.left));
        break;
      case NORMAL:
        value = SpecialFunction.normal(
                evaluate(node.left));
        break;

      case POISSON:
        value = SpecialFunction.poisson(
                (int) (evaluate(node.left) + 0.01),
                evaluate(node.right));
        break;
      case POISSONC:
        value = SpecialFunction.poissonc(
                (int) (evaluate(node.left) + 0.01),
                evaluate(node.right));
        break;

      case CHISQ:
        value = SpecialFunction.chisq(
                evaluate(node.left),
                evaluate(node.right));
        break;
      case CHISQC:
        value = SpecialFunction.chisqc(
                evaluate(node.left),
                evaluate(node.right));
        break;
      case IGAM:
        value = SpecialFunction.igam(
                evaluate(node.left),
                evaluate(node.right));
        break;
      case IGAMC:
        value = SpecialFunction.igamc(
                evaluate(node.left),
                evaluate(node.right));
        break;

      case ATAN2:
        value = Math.atan2(evaluate(node.left),
                evaluate(node.right));
        break;

      case JN:
        value = SpecialFunction.jn(
                (int) (evaluate(node.left) + 0.01),
                evaluate(node.right));
        break;

      case YN:
        value = SpecialFunction.yn(
                (int) (evaluate(node.left) + 0.01),
                evaluate(node.right));
        break;

      default:
        throw new Exception(
                "evaluate: Failed because of an unknown intrinsic!");
    }

    return value;

  }

  /*
  ** The basic parsing mechanism failes to recognise precedence.
  ** this method reorganises the nodes into ascending precedence.
  ** That is nodes with higher precedence are pushed further down the tree
  ** this means they get evaluated first.
  **
  ** Precedence
  **           Grouping
  **           Unary minus/plus
  **           Power
  **           Multiplication/Division
  **           Addition/Subtraction
  **           Variables, Intrinsic Functions, Constants
  */


  private void reOrderNodes(Node node) {
    Node right = null;
    Node left = null;

    if (node == null) return;

    right = node.right;
    left = node.left;

    if (right != null && right.type == Node.GROUP) {
      reOrderNodes(right);
    } else if (left != null && left.type == Node.GROUP) {
      reOrderNodes(left);
    }
    if (node.type == Node.GROUP) {
      reOrderNodes(left);
    } else if (node.type == Node.OP && right.type == Node.OP) {
      if (node.precedence >= right.precedence) {

        Node newnode = new Node(node);

        newnode.right = right.left;

        node.replace(right);

        node.left = newnode;

        right = null;

        reOrderNodes(node);
      }
    }

  }

}


class Node extends Object {
  public static final int OP = 0;
  public static final int VALUE = 1;
  public static final int INTRINSIC = 2;
  public static final int NULL = 3;
  public static final int INDEPENDENT = 4;
  public static final int GROUP = 5;
  public static final int PARAMETER = 6;

  public static final int P0 = 0;
  public static final int P1 = 1;
  public static final int P2 = 2;
  public static final int P3 = 3;
  public static final int P4 = 4;
  public static final int P5 = 5;

  int type;
  Node left;
  Node right;
  int op;
  double value;
  int precedence;


  public Node() {
    type = NULL;
    left = null;
    right = null;
    op = NULL;
    value = 0.0;
    precedence = P0;
  }

  public Node(Node n) {
    replace(n);
  }

  public void replace(Node n) {
    if (n == null) return;
    op = n.op;
    type = n.type;
    left = n.left;
    right = n.right;
    value = n.value;
    precedence = n.precedence;
  }

  public void indent(int ind) {
    for (int i = 0; i < ind; i++) System.out.print(" ");
  }

  public void print(int indentLevel) {
    indent(indentLevel);
    Graph2D.out.println("NODE type=" + type);
    indent(indentLevel);
    Graph2D.out.println("     prec=" + precedence);
    indent(indentLevel);
    switch (type) {
      case Node.VALUE:
        Graph2D.out.println("     value=" + value);
        break;
      case Node.INDEPENDENT:
        Graph2D.out.println("     variable=" + op);
        break;
      default:
        Graph2D.out.println("     op=" + op);
        if (left != null) left.print(indentLevel + 5);
        if (right != null) right.print(indentLevel + 5);
        break;
    }
  }

}
