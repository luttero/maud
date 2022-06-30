package it.unitn.ing.rista.io;

import java.io.*;
import java.util.*;
import org.json.simple.*;
import org.json.simple.parser.*;

public class JSONFileImport {

	public JSONObject jsonObject = null;

	public void readFile(String filename) {
		JSONParser parser = new JSONParser();
		try {
			JSONObject jsonObject = (JSONObject) parser.parse(new FileReader(filename));

			JSONObject analysis = (JSONObject) jsonObject.get("radiographema::core::Analysis");
/*			JSONObject preferences = (JSONObject) analysis.get("radiographema::core::Preferences|Analysis preferences");
			String zdisp = (String) preferences.get("default_Z_displacement_num");
			System.out.println("From json: " + zdisp);*/

			Iterator iter = analysis.entrySet().iterator();
			while(iter.hasNext()) {
				Map.Entry entry = (Map.Entry) iter.next();
				String key = entry.getKey().toString();
				String value = entry.getValue().toString();
				System.out.println(key);
			}

/*			JSONArray subjects = (JSONArray)jsonObject.get("Subjects");
			System.out.println("Name: " + name);
			System.out.println("Course: " + course);
			System.out.println("Subjects:");
			Iterator iterator = subjects.iterator();
			while (iterator.hasNext()) {
				System.out.println(iterator.next());
			}*/


		} catch(Exception e) {
			e.printStackTrace();
		}

	}

	public JSONArray getObjectsArrayFor(String name) {
		return (JSONArray)jsonObject.get(name);
	}

	public JSONArray getPhasesList() {
		return (JSONArray)jsonObject.get("radiographema::core::Analysis");
	}


}
