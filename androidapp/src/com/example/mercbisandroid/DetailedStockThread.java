package com.example.mercbisandroid;

import java.io.BufferedReader;
import java.io.IOException;

import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;


public class DetailedStockThread extends AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> {

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
         String fixedUrl = "http://mercury.dyndns.org:8080/JAXRS-BISystem/api/stocks/month/";
         String Symbol = MainActivity.StockSymbol;
         String RealUrl = fixedUrl + Symbol;
         
         System.out.println(RealUrl);
         
        try {    
        	    
                 URL url = new URL(RealUrl);
                
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
                        
                    	   JSONArray JArray = new JSONArray(jsonLine);
                          
             //              System.out.println(JArray.getString(0));
                       
                            for (int i = 0; i < JArray.length(); i++) {
                            	
                            	JSONObject jsonObjStock = new JSONObject(new String(JArray.getString(i)));
                                JSONObject JSONObjStockVal = new JSONObject(jsonObjStock.getString("value"));
                            
                            	
                
                
                 JSONLIST.add(JSONObjStockVal);
                         }
                        
                         } catch (JSONException e) {
                         // TODO Auto-generated catch block
                         e.printStackTrace();
                        
                         }
                        
                        
                         return JSONLIST;
                
        
                } catch (Exception e) {
                 e.printStackTrace();
                }       
        
        return null;

}
}