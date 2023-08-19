package it.unitn.ing.wizard.LoskoWizard;

import it.unitn.ing.rista.diffr.FilePar;
import it.unitn.ing.rista.diffr.XRDcat;
import it.unitn.ing.rista.io.cif.CIFItem;
import it.unitn.ing.rista.io.cif.CIFtoken;
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Vector;

public class LoskoConfigData {

	//	public static final String INSTRUMENT_FILE_DIR = "Instrument file directory";
	public static final String DATA_FILE_DIR = "Losko images directory";
	public static final String DETECTOR_PANEL_ENABLED = " enabled";

	public static final String LOSKO_PANEL_TYPE = "panel_type";
	public static final String LOSKO_DETECTORS_NUMBER = "detectors_number";
	public static final String LOSKO_COORD_X = "detector_origin_x";
	public static final String LOSKO_COORD_Y = "detector_origin_y";
	public static final String LOSKO_PANEL_WIDTH = "detector_width";
	public static final String LOSKO_PANEL_HEIGHT = "detector_height";
	public static final String LOSKO_PANEL_DARK_NUMBER = "pedestals_image_number";
	public static final String LOSKO_PANEL_DARK_ROTATION = "pedestals_image_rotation";
	public static final String LOSKO_PANEL_ROTATION = "panel_rotation";

	public static final String[] DETECTOR_XMIN_XMAX = {
			" 2theta min",
			" 2theta max"};

	public static String getPropertyValue(String property, String defaultvalue) {
		property = Misc.toStringNoBlank(property);
		return MaudPreferences.getPref("LoskoWizard." + property, defaultvalue);
	}

	public static void setPropertyValue(String property, String value) {
		property = Misc.toStringNoBlank(property);
		MaudPreferences.setPref("LoskoWizard." + property, value);
	}

	public static int getPropertyValue(String property, int defaultvalue) {
		property = Misc.toStringNoBlank(property);
		return MaudPreferences.getInteger("LoskoWizard." + property, defaultvalue);
	}

	public static double getPropertyValue(String property, double defaultvalue) {
		property = Misc.toStringNoBlank(property);
		return MaudPreferences.getDouble("LoskoWizard." + property, defaultvalue);
	}

	public static void setPropertyValue(String property, int value) {
		property = Misc.toStringNoBlank(property);
		MaudPreferences.setPref("LoskoWizard." + property, value);
	}

	public static void writeConfig() {
	}

	public static void readLoskoConfigDataFromFile(String filename) {

//		for (int i = 0; i < diclist.length; i++)
//			System.out.println(i + " " + diclist[i]);
//		System.out.println(diclist[DETECTOR_NUMBER] + " " + diclist[ORIGIN_X]);

		if (filename != null) {

			if (detectorPanels != null)
				detectorPanels.removeAllElements();
			else
				detectorPanels = new Vector<LoskoDetectorPanel>(0, 1);

			if (detectorTypes != null)
				detectorTypes.removeAllElements();
			else
				detectorTypes = new Vector<LoskoDetectorType>(0, 1);

			String thecife;
			int newtoken, tokentype;

			BufferedReader reader = Misc.getReader(filename);
			if (reader != null) {
				CIFtoken ciffile = new CIFtoken(reader);
				try {
					do {
						tokentype = ciffile.nextToken();
//					System.out.println(ciffile.thestring + " " + tokentype + " == " + CIFtoken.TT_LOOP + " or == " + CIFtoken.TT_CIFE);
						switch (tokentype) {
							case CIFtoken.TT_DATA:
								// new block of data
								break;
							case CIFtoken.TT_GLOB:
								// global data
								break;
							case CIFtoken.TT_LOOP:
								// data loop
								Vector itemlistv = new Vector(0, 1);
								Vector cifelistv = new Vector(0, 1);
								newtoken = ciffile.nextToken();
								while (newtoken == CIFtoken.TT_CIFE) {
									itemlistv.addElement(ciffile.thestring);
									newtoken = ciffile.nextToken();
//								System.out.println(ciffile.thestring);
								}
								int loopitem = itemlistv.size();
//								System.out.println("Reading loop with " + loopitem + " elements");
								if (loopitem > 0) {
									while (FilePar.isValidToken(newtoken)) {
										cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(0),
												ciffile.thestring, ciffile.thestringerror, ciffile.free));
										for (int i = 1; i < loopitem; i++) {
											ciffile.nextToken();
//										System.out.println(ciffile.thestring);
											cifelistv.addElement(new CIFItem((String) itemlistv.elementAt(i),
													ciffile.thestring, ciffile.thestringerror, ciffile.free));
										}
										newtoken = ciffile.nextToken();
//									System.out.println(ciffile.thestring);
									}
								}
								ciffile.pushBack();
//							System.out.println("Pushback: " + ciffile.thestring);
								setLoop(cifelistv, loopitem);
								cifelistv.removeAllElements();
								itemlistv.removeAllElements();
								break;
							case CIFtoken.TT_CIFE:
								// CIF item
								thecife = ciffile.thestring;
								newtoken = ciffile.nextToken();
//							System.out.println(ciffile.thestring);
								if (FilePar.isValidToken(newtoken))
									setField(thecife, ciffile.thestring);
								else {
									ciffile.pushBack();
//								System.out.println("Pushback: " + ciffile.thestring);
								}
								break;
							default:
							{
							}
						}
					} while (tokentype != CIFtoken.TT_EOF);

				} catch (IOException e) {
					System.out.println("Error loading cif file!");
				}
				try {
					reader.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
				System.out.println("Loaded " + detectorPanels.size() + " panels and " + detectorTypes.size() + " detector types");
				for (int i = 0; i < detectorPanels.size(); i++) {
					LoskoDetectorPanel panel = detectorPanels.elementAt(i);
					System.out.println("Panel " + panel.panelName + ", type: " + panel.panelType + " with " + panel.detectorsNumber + " sensors");
				}
				for (int i = 0; i < detectorTypes.size(); i++) {
					LoskoDetectorType type = detectorTypes.elementAt(i);
					int maxWidth = 0;
					int maxHeight = 0;
					for (int j = 0; j < type.sensors.size(); j++) {
						LoskoDetectorSensor sensor = type.sensors.elementAt(j);
						int width = sensor.detector_width + sensor.origin_x;
						if (width > maxWidth)
							maxWidth = width;
						int height = sensor.detector_height + sensor.origin_y;
						if (height > maxHeight)
							maxHeight = height;
					}
					type.width = maxWidth;
					type.height = maxHeight;
				}
				for (int i = 0; i < detectorTypes.size(); i++) {
					LoskoDetectorType type = detectorTypes.elementAt(i);
					System.out.println("Detector type " + type.typeName + " with " + type.sensors.size() + " sensors");
					for (int j = 0; j < type.sensors.size(); j++) {
						LoskoDetectorSensor sensor = type.sensors.elementAt(j);
						System.out.println("Sensor " + sensor.detector_number + ": " + sensor.detector_width + " x " + sensor.detector_height +
								" at " + sensor.origin_x + ", " + sensor.origin_y);
					}
				}
			}
		}

	}

	public static String[] diclist = {"_radiation_energy_KeV", "_pd_spec_orientation_omega", "_calibration_data_directory",
			"_panel_name", "_panel_type", "_detectors_number", "_pedestals_file",
			"_pixels_status_file", "_panel_rotation", "_pixel_size_mm_x", "_pixel_size_mm_y", "_panel_distance",
			"_center_x", "_center_y", "_panel_tilting", "_panel_2theta", "_panel_omegaDN",
			"_detector_number", "_origin_x", "_origin_y", "_detector_width", "_detector_height",
			"_pedestal_number", "_pedestal_rotation", "_pedestal_shift_x", "_pedestal_shift_y"};

	public static final int RAD_ENERGY_KEV = 0, OMEGA = 1, CAL_DATA_DIR = 2,
			PANEL_NAME = 3, PANEL_TYPE = 4, DETECTORS_NUMBER = 5, PEDESTAL_FILE = 6,
			PIXEL_STATUS_FILE = 7, PANEL_ROTATION = 8, PIXEL_SIZE_MM_X = 9, PIXEL_SIZE_MM_Y = 10, PANEL_DISTANCE = 11,
			CENTER_X = 12, CENTER_Y = 13, PANEL_TILTING = 14, PANEL_2THETA = 15, PANEL_OMEGADN = 16,
			DETECTOR_NUMBER = 17, ORIGIN_X = 18, ORIGIN_Y = 19, DETECTOR_WIDTH = 20, DETECTOR_HEIGHT = 21,
			PEDESTAL_NUMBER = 22, PEDESTAL_ROTATION = 23, PEDESTAL_SHIFT_X = 24, PEDESTAL_SHIFT_Y = 25;

	public static int ciftonumber(String cif) {
		int number = -1;
		for (int i = 0; i < diclist.length; i++)
			if (cif.equalsIgnoreCase(diclist[i]))
				return i;
		return number;
	}

	public static double radiationKeV = 10.217;
	public static double omega = 72.7;
	public static String calibrationDirectory = "";
	public static String filenameToSave = getPropertyValue("UnrolledImagesDatafile", "Filename for saving unrolled images datafiles....");
	public static String filenameTemplate = getPropertyValue("LoskoDefaultTemplate", "Template file for analysis....");
	public static int version = 0;

	public static Vector<LoskoDetectorPanel> detectorPanels = null;
	public static Vector<LoskoDetectorType> detectorTypes = null;

	public static LoskoDetectorType getDetectorType(String typeName) {
		if (detectorTypes != null) {
			for (int i = 0; i < detectorTypes.size(); i++) {
				LoskoDetectorType type = detectorTypes.elementAt(i);
				if (type.typeName.equalsIgnoreCase(typeName))
					return type;
			}
		}
		return null;
	}

	public static int setField(String cif, String astring) {
		cif = XRDcat.validateCIF(cif);
		int index = ciftonumber(cif);

		switch (index) {
			case RAD_ENERGY_KEV:
				radiationKeV = Double.parseDouble(astring);
				break;
			case OMEGA:
				omega = Double.parseDouble(astring);
				break;
			case CAL_DATA_DIR:
				calibrationDirectory = astring;
				break;
			default: {}
		}

		return index;
	}

	public static void setLoop(Vector avector, int loopItemsNumber) {

		XRDcat.validateCIF(avector);

		int loopitem = avector.size();
		int i = 0, index;
		CIFItem item;
		double rotation;

		boolean head = true;
		int headIndex = -1;
		LoskoDetectorPanel panel = null;
		LoskoDetectorType type = null;
		LoskoDetectorSensor sensor = null;

		if (loopitem > 0) {
			while (i < loopitem) {
				item = (CIFItem) avector.elementAt(i);
				index = ciftonumber(item.cif);
//	      System.out.println(item.cif + " " + index + " " + item.thestring);

				if (head) {
					headIndex = index;
					head = false;
//					System.out.println("Head is " + diclist[headIndex]);
				}
//				if (index >= 0)
//					System.out.println("Assigning " + diclist[index]);
				switch (index) {
					case PANEL_NAME:
						panel = new LoskoDetectorPanel(item.thestring);
						detectorPanels.add(panel);
//						System.out.println("Added panel " + item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PANEL_TYPE:
						if (headIndex == PANEL_TYPE) {
							type = getDetectorType(item.thestring);
							if (type == null) {
								type = new LoskoDetectorType(item.thestring);
								detectorTypes.add(type);
//								System.out.println("Added type " + item.thestring);
							}
						} else {
							if (panel != null) {
								panel.panelType = item.thestring;
								type = getDetectorType(item.thestring);
								if (type == null) {
									type = new LoskoDetectorType(item.thestring);
									detectorTypes.add(type);
//									System.out.println("Added detector type " + item.thestring);
								}
							}
						}
						avector.removeElementAt(i);
						loopitem--;
						break;
					case DETECTORS_NUMBER:
						if (panel != null) {
							panel.detectorsNumber = Integer.parseInt(item.thestring);
							if (type != null) {
								while (type.sensors.size() < panel.detectorsNumber) {
									sensor = new LoskoDetectorSensor(panel.panelType);
									int number = type.sensors.size();
									sensor.detector_number = number;
									type.sensors.add(sensor);
								}
							}
						}
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PEDESTAL_FILE:
						if (panel != null)
							panel.pedestalsFile = item.thestring;
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PIXEL_STATUS_FILE:
						if (panel != null)
							panel.pixelsStatusFile = item.thestring;
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PANEL_ROTATION:
						if (panel != null) {
							rotation = Double.parseDouble(item.thestring);
							while (rotation > 180.0)
								rotation -= 360;
							while (rotation < -180)
								rotation += 360;
							if (rotation > 85.0 && rotation < 95.0)
								panel.panelRotation = LoskoDetectorPanel.ROT_CW;
							else if (rotation > -95.0 && rotation < -85.0)
								panel.panelRotation = LoskoDetectorPanel.ROT_CCW;
							else if (rotation < -175.0 || rotation > 175.0)
								panel.panelRotation = LoskoDetectorPanel.ROT_180;
							else
								panel.panelRotation = LoskoDetectorPanel.ROT_NO;
						}
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PIXEL_SIZE_MM_X:
						if (panel != null)
							panel.pixel_size_mm_x = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PIXEL_SIZE_MM_Y:
						if (panel != null)
							panel.pixel_size_mm_y = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PANEL_DISTANCE:
						if (panel != null)
							panel.panel_distance = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case CENTER_X:
						if (panel != null)
							panel.center_x = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case CENTER_Y:
						if (panel != null)
							panel.center_y = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PANEL_TILTING:
						if (panel != null)
							panel.panel_tilting = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PANEL_2THETA:
						if (panel != null)
							panel.panel_2theta = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PANEL_OMEGADN:
						if (panel != null)
							panel.panel_omegaDN = Double.parseDouble(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case DETECTOR_NUMBER:
						int indexD = Integer.parseInt(item.thestring);
//						System.out.println("Sensor number: " + indexD);
						if (type != null) {
//							System.out.println("Before: " + type.typeName + " " + type.sensors.size());
							while (type.sensors.size() <= indexD) {
								sensor = new LoskoDetectorSensor(type.typeName);
								int number = type.sensors.size();
								sensor.detector_number = number;
								type.sensors.add(sensor);
							}
							sensor = type.sensors.elementAt(indexD);
//							System.out.println("After: " + sensor.panelType + " " + type.sensors.size());
						} else {
							System.out.println("Warning: no panel type selected!");
						}
						avector.removeElementAt(i);
						loopitem--;
						break;
					case ORIGIN_X:
//						System.out.println("Sensor: " + sensor + ", x = " + item.thestring);
						if (sensor != null)
							sensor.origin_x = Integer.parseInt(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case ORIGIN_Y:
//						System.out.println("Sensor: " + sensor + ", y = " + item.thestring);
						if (sensor != null)
							sensor.origin_y = Integer.parseInt(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case DETECTOR_WIDTH:
//						System.out.println("Sensor: " + sensor + ", w = " + item.thestring);
						if (sensor != null) {
							sensor.detector_width = Integer.parseInt(item.thestring);
							if (sensor.detector_width > 350) // full sensor with gap in between
								version = 1;
						}
						avector.removeElementAt(i);
						loopitem--;
						break;
					case DETECTOR_HEIGHT:
//						System.out.println("Sensor: " + sensor + ", h = " + item.thestring);
						if (sensor != null) {
							sensor.detector_height = Integer.parseInt(item.thestring);
							if (sensor.detector_height > 350) // full sensor with gap in between
								version = 1;
						}
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PEDESTAL_NUMBER:
//						System.out.println("Sensor: " + sensor + ", n = " + item.thestring);
						if (sensor != null)
							sensor.pedestal_number = Integer.parseInt(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PEDESTAL_ROTATION:
//						System.out.println("Sensor: " + sensor + ", r = " + item.thestring);
						if (sensor != null) {
							rotation = Double.parseDouble(item.thestring);
							while (rotation > 180.0)
								rotation -= 360;
							while (rotation < -180)
								rotation += 360;
							if (rotation > 85.0 && rotation < 95.0)
								sensor.pedestal_rotation = LoskoDetectorPanel.ROT_CW;
							else if (rotation > -95.0 && rotation < -85.0)
								sensor.pedestal_rotation = LoskoDetectorPanel.ROT_CCW;
							else if (rotation < -175.0 || rotation > 175.0)
								sensor.pedestal_rotation = LoskoDetectorPanel.ROT_180;
							else
								sensor.pedestal_rotation = LoskoDetectorPanel.ROT_NO;
						}
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PEDESTAL_SHIFT_X:
//						System.out.println("Sensor: " + sensor + ", sx = " + item.thestring);
						if (sensor != null)
							sensor.pedestal_shift_x = Integer.parseInt(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					case PEDESTAL_SHIFT_Y:
//						System.out.println("Sensor: " + sensor + ", sy = " + item.thestring);
						if (sensor != null)
							sensor.pedestal_shift_y = Integer.parseInt(item.thestring);
						avector.removeElementAt(i);
						loopitem--;
						break;
					default: { i++; }
				}

			} // end of while (i < loopitem)

		} // end of if (loopitem>0)

	}

}
