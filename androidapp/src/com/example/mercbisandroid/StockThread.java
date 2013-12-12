package com.example.mercbisandroid;

import java.io.BufferedReader;
import java.io.IOException;
// import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;

/**
 * @author Rickard Bremer
 *
 */

public class StockThread extends AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> {

protected void onProgressUpdate(Integer... progress) {
   // setProgressPercent(progress[0]);
}

protected void onPostExecute(Long result) {
 // showDialog("Downloaded " + result + " bytes");
}

@Override
protected ArrayList<Object> doInBackground(ArrayList<Object>... params) {
        
         ArrayList<Object> JSONLIST = new ArrayList<Object>();
         BufferedReader reader = null;
         StringBuffer jsonBuffer = new StringBuffer();
         String jsonLine = "";
         String line = "";
        
        try {
                 URL url = new URL("http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse?startkey=\"1384142400000\"&endkey=\"1384172149000\"");
                
                 HttpURLConnection con = (HttpURLConnection) url.openConnection();
                
                 try {
                         reader = new BufferedReader(new InputStreamReader(con.getInputStream()));
                        
                        
                         while ((line = reader.readLine()) != null) {
                         jsonBuffer.append(line);
                         }
                         }
                                 catch (IOException e) {
                         e.printStackTrace();
                         }
                
                         finally {
                         if (reader != null) {
                         try {
                         reader.close();
                         }
                         catch (IOException e) {
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
                         JSONLIST.add(JSONObjStockVal);
                         
                         }
                        
                         } catch (JSONException e) {
                         
                         e.printStackTrace();
                        
                         }
                        
                       
                         return JSONLIST;
                
        
                } catch (Exception e) {
                 e.printStackTrace();
                }        
        
        return null;
}

}