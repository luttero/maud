/*
 * @(#)XMLReader.java created Jan 11, 2005 Riva Del Garda
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.io;

import org.w3c.dom.*;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import java.io.*;

import it.unitn.ing.rista.util.Misc;


/**
 * The XMLReader is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:58 $
 * @since JDK1.1
 */

public class XMLReader {

  Document dom = null;

  public void readFile(File inputFile) {
    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    try {
      DocumentBuilder docBuilder = dbf.newDocumentBuilder();
      dom = docBuilder.parse(inputFile);
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public void readFile(InputStream inputFile) {
    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    try {
      DocumentBuilder docBuilder = dbf.newDocumentBuilder();
      dom = docBuilder.parse(inputFile);
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public void readFile(String uri) {
    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    try {
      DocumentBuilder docBuilder = dbf.newDocumentBuilder();
      dom = docBuilder.parse(uri);
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public void readString(String string) {
    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
    try {
      DocumentBuilder docBuilder = dbf.newDocumentBuilder();
      StringReader reader = new StringReader( string );
      InputSource inputSource = new InputSource( reader );
      dom = docBuilder.parse(inputSource);
      reader.close();
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
  }

  public Document getDocument() {
    return dom;
  }

  public NodeList getElementsByTagName(String tagname) {
    return dom.getElementsByTagName(tagname);
  }

  public Element getRootNode() {
    return dom.getDocumentElement();
  }

  public void checkTree() {
    checkTree(getRootNode());
  }

/*  public void checkTree(Element element) {
    System.out.println("Element: " + element.getTagName());
    NodeList nodelist = element.getChildNodes();
    int numberOfElements = nodelist.getLength();
    System.out.println("Number of childs: " + numberOfElements);
    for (int i = 0; i < numberOfElements; i++) {
      Node node = nodelist.item(i);
      if (node instanceof Element)
        checkTree((Element) node);
      else
        checkTree(node);
    }
    System.out.println("End of nodelist for element " + element.getTagName());
  }*/

  public void checkTree(Node element) {
    if (element.getNodeName().equals("#text")) {
      System.out.println("Text value: " + element.getNodeValue());
    } else {
      System.out.println("Element: " + element.getNodeName());
      NodeList nodelist = element.getChildNodes();
      int numberOfElements = nodelist.getLength();
      System.out.println("Number of childs: " + numberOfElements);
      for (int i = 0; i < numberOfElements; i++) {
        Node node = nodelist.item(i);
        checkTree(node);
      }
      System.out.println("End of nodelist for element " + element.getNodeName());
    }
  }

  public String getValue(Node element) {
    NodeList nodelist = element.getChildNodes();
    Node node = nodelist.item(0);
    return node.getNodeValue();
  }

  public Element getElementById(String ID) {
    Element element = dom.getElementById(ID);
    return element;
  }
}
