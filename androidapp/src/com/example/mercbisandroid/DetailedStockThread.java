
package com.example.mercbisandroid;

import java.io.BufferedReader;
import java.io.IOException;

import java.io.InputStreamReader;
import java.math.BigInteger;
import java.net.HttpURLConnection;
import java.net.URL;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;


/**
 * @author Rickard Bremer
 * This class downloads the detailed stock data when entering the detailed stock overview.
 */

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
         String RealUrl = "http://mercury.dyndns.org:5984/mercury/_design/bi/_view/nyse_stock?startkey=[%22"+MainActivity.StockSymbol+"%22,%22" + MainActivity.StockTime + "%22]&endkey=[%22"+MainActivity.StockSymbol+"%22,%22"+ System.currentTimeMillis() +"%22]";
    
       //  System.out.println(RealUrl);
         
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
                        
                    	  JSONObject jsonObjMain = new JSONObject(jsonLine);
                          JSONArray jArray = jsonObjMain.getJSONArray("rows");
                          
                            for (int i = 0; i < jArray.length(); i++) {
                            	
                            	JSONObject jsonObjStock = new JSONObject(new String(jArray.getString(i)));
                                JSONObject JSONObjStockVal = new JSONObject(jsonObjStock.getString("value"));
                            
                            	
                //System.out.println(JSONObjStockVal);
                
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