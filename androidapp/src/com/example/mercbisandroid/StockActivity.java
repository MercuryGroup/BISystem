package com.example.mercbisandroid;

import java.util.ArrayList;
import java.util.concurrent.ExecutionException;

import org.afree.chart.ChartFactory;
import org.afree.chart.plot.PlotOrientation;
import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;
import org.json.JSONException;
import org.json.JSONObject;

import android.app.Activity;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.ViewGroup;

public class StockActivity extends Activity {
	
	public AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> DetailedStockArray;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {   
       super.onCreate(savedInstanceState);    
       setContentView(R.layout.activity_stock);
       DetailedStockArray = new DetailedStockThread().execute();
     
       CategoryDataset dataset = CreateDataSet(DetailedStockArray);
       ViewGroup viewGroup = (ViewGroup)getWindow().getDecorView().findViewById(android.R.id.content);
       ChartView chartView = new ChartView(this);
       chartView.drawChart(ChartFactory.createLineChart(MainActivity.StockSymbol,"Date","Value",dataset,PlotOrientation.VERTICAL,true,true,false));       

       viewGroup.addView(chartView);
       
   }

public static CategoryDataset CreateDataSet(AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> detailedStockArray){
	
	DefaultCategoryDataset dataset = new DefaultCategoryDataset();
	
	try {
		System.out.println(detailedStockArray.get().size());
	} catch (InterruptedException e1) {
		// TODO Auto-generated catch block
		e1.printStackTrace();
	} catch (ExecutionException e1) {
		// TODO Auto-generated catch block
		e1.printStackTrace();
	}
	
	try {
		for(int i = 0; i < detailedStockArray.get().size(); i++){
		 
		JSONObject JOBJ = new JSONObject(detailedStockArray.get().get(i).toString());
		
		
		String latest = JOBJ.getString("latest");
		String updated = JOBJ.getString("updated");
		System.out.println("latest :" + latest + " # "+" updated : " + updated);
		
		//dataset.addValue(Float.parseFloat(latest), "Value", updated);
		dataset.addValue(Float.parseFloat(latest), "Value", "updated");
		}
	} catch (NumberFormatException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (InterruptedException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (ExecutionException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (JSONException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
	return dataset; 
}


}
