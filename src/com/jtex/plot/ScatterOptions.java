/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.jtex.plot;

import java.awt.Color;
import java.awt.Stroke;

/**
 *
 * @author flb
 */
public class ScatterOptions {

    private Color lineColor;
    private Color markerEdgeColor;
    private Color markerFaceColor;

    private Stroke lineStroke;
    private Shapes.Named marker;

    private double markerSize;

    public ScatterOptions() {
        this.lineStroke = null;
        this.marker = Shapes.Named.CIRCLE;

        this.markerFaceColor = Color.BLUE;
        this.lineColor = Color.BLUE;
        this.markerSize = 2D;
    }

    public ScatterOptions(Stroke lineStroke, Shapes.Named marker) {
        this.lineStroke = lineStroke;
        this.marker = marker;
        this.markerFaceColor = Color.BLUE;
        this.lineColor = Color.BLUE;
        this.markerSize = 2D;
    }

    public ScatterOptions(Shapes.Named marker, double markersize) {
        this.lineStroke = null;
        this.marker = marker;
        this.markerFaceColor = Color.BLUE;
        this.lineColor = Color.BLUE;
        this.markerSize = markersize;
    }

    public Stroke getLineStroke() {
        return lineStroke;
    }

    public Shapes.Named getMarker() {
        return marker;
    }

    public void setLineStroke(Stroke lineStroke) {
        this.lineStroke = lineStroke;
    }

    public void setMarker(Shapes.Named marker) {
        this.marker = marker;
    }

    public double getMarkerSize() {
        return markerSize;
    }

    public void setLineColor(Color lineColor) {
        this.lineColor = lineColor;
    }

    public void setMarkerEdgeColor(Color markerEdgeColor) {
        this.markerEdgeColor = markerEdgeColor;
    }

    public void setMarkerFaceColor(Color markerFaceColor) {
        this.markerFaceColor = markerFaceColor;
    }

    public void setMarkerSize(double markerSize) {
        this.markerSize = markerSize;
    }

    public Color getLineColor() {
        return lineColor;
    }

    public Color getMarkerEdgeColor() {
        return markerEdgeColor;
    }

    public Color getMarkerFaceColor() {
        return markerFaceColor;
    }

}
