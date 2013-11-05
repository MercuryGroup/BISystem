package com.example.mercbisandroid;

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

public class httpThread extends AsyncTask<Void, Void, Void> {

protected void onProgressUpdate(Integer... progress) {
   // setProgressPercent(progress[0]);
}

protected void onPostExecute(Long result) {
 //   showDialog("Downloaded " + result + " bytes");
}

@Override
protected Void doInBackground(Void... arg0) {
	
	try {
		  URL url = new URL("http://46.194.18.251:5984/mercury/_design/bi/_view/lse");
		  
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

	  JSONObject jsonObj = jsonArray.getJSONObject(i);

	  
	  String id = jsonObj.getString("id");
//	  String name = jsonObj.getString("name");
//	  String city = jsonObj.getString("city");
//	  String gender = jsonObj.getString("gender");
//	  int age = jsonObj.getInt("id");

	  System.out.println("id : " + id);
	  

	  }
	  } catch (JSONException e) {
	  // TODO Auto-generated catch block
	  e.printStackTrace();
	  
	  }
	  }

}
	

