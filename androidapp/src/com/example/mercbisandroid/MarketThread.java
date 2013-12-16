package com.example.mercbisandroid;
/**
 * @author Rickard Bremer
 *
 */
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;

public class MarketThread extends AsyncTask<Void, Void, Void> {

protected void onProgressUpdate(Integer... progress) {
   // setProgressPercent(progress[0]);
}

protected void onPostExecute(Long result) {
 //   showDialog("Downloaded " + result + " bytes");
}

@Override
protected Void doInBackground(Void... arg0) {
	
	try {
		  URL url = new URL("http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=\"1383565321852\"&endkey=\"1383565328964\"");
		  
		  HttpURLConnection con = (HttpURLConnection) url
		    .openConnection();
		  readStream(con.getInputStream());
		  } catch (Exception e) {
		  e.printStackTrace();
		}	
	
	return null;
}

private void readStream(InputStream in) {
	  BufferedReader reader = null;
	  StringBuffer jsonBuffer = new StringBuffer();
	  String jsonLine = "";
	  
	  try {
	    reader = new BufferedReader(new InputStreamReader(in));
	    String line = "";
	    
	    while ((line = reader.readLine()) != null) {
	       jsonBuffer.append(line);
	      // System.out.println(line);
	    }
	  } catch (IOException e) {
	    e.printStackTrace();
	  } finally {
	    if (reader != null) {
	      try {
	        reader.close();
	      } catch (IOException e) {
	        e.printStackTrace();
	      }
	    }
	  }
	
		jsonLine = jsonBuffer.toString();
		
	  try {
	  
	  JSONObject jsonObjMain = new JSONObject(jsonLine);
	  JSONArray jsonArray = jsonObjMain.getJSONArray("rows");
	  
	  for (int i = 0; i < jsonArray.length(); i++) {
		  
	  JSONObject jsonObjStock = new JSONObject(new String(jsonArray.getString(i)));
      
	  JSONObject JSONObjStockVal = new JSONObject(jsonObjStock.getString("value"));
	  
	  String symbol = JSONObjStockVal.getString("symbol");
	  String name = JSONObjStockVal.getString("name");
	  String change = JSONObjStockVal.getString("change");
	  String latest = JSONObjStockVal.getString("latest");
	  String percent = JSONObjStockVal.getString("percent");
	  String volume = JSONObjStockVal.getString("volume");
	  String market = JSONObjStockVal.getString("market");
	  String updated = JSONObjStockVal.getString("updated");
	  String openVal = JSONObjStockVal.getString("openVal");
	  

	  System.out.println(symbol + " " + " " + name + " " + change + " " + latest + " " + percent + " " + volume + " " + market + " " + updated + "" + openVal + "\n");
	
	  

	  }
	  } catch (JSONException e) {
	  // TODO Auto-generated catch block
	  e.printStackTrace();
	  
	  }
	  }



}
	

