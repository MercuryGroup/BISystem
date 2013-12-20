
package com.example.mercbisandroid;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutionException;

import org.afree.chart.ChartFactory;
import org.afree.chart.plot.PlotOrientation;
import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;
import org.afree.data.xy.DefaultHighLowDataset;
import org.json.JSONException;
import org.json.JSONObject;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.RadioButton;
import android.widget.TextView;
import android.widget.Toast;
/*import org.afree.chart.ChartFactory;
import org.afree.chart.plot.PlotOrientation;
import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;*/

/**
 * @author Rickard Bremer
 * This activity download a detailed stock list, presents the diffrent graph's in the detailed stock view.
 * It also displays the diffrent new available for the stock. There is also a portfolio button, which you can sue
 * to add a stock to the portfolio.
 */

public class StockActivity extends Activity {
	
	
	JSONObject JsonNewsObject;
	public  ArrayList<JSONObject> JsonNewsList = new ArrayList<JSONObject>();
	
    String[] title;
    String[] links;
    String chart = "linechart";
    
    DefaultHighLowDataset datasetCandle = null;
    CategoryDataset dataset = null;
    ChartView chartView = null;
    ViewGroup viewGroup = null;
    
    AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> stockArray;


protected void onCreate(Bundle savedInstanceState){
		super.onCreate(savedInstanceState);
		
		setContentView(R.layout.activity_stock);
		AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> newsArray = new NewsThread().execute();
		
		this.stockArray = new DetailedStockThread().execute();
		
		try {
			title = new String[newsArray.get().size()];
			links = new String[newsArray.get().size()];
		} catch (InterruptedException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (ExecutionException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		try {
			
			for(int i = 0; i < newsArray.get().size(); i++){
				
				String 	JsonNewsString = newsArray.get().get(i).toString();
				try {
					JsonNewsObject = new JSONObject(JsonNewsString);
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				JsonNewsList.add(JsonNewsObject);
			}
		} catch (InterruptedException e) {
		
			e.printStackTrace();
		} catch (ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		for (int i = 0; i < JsonNewsList.size(); i++){
				try {
					title[i] = JsonNewsList.get(i).getString("title");
					links[i] = JsonNewsList.get(i).getString("link");
				} catch (JSONException e) {
					e.printStackTrace();
				}
				
			
		}
		
		
				TextView stockInfo =(TextView)findViewById(R.id.stockInfo); 
				try {
					stockInfo.setText("   Name  : " + MainActivity.StockObject.getString("name") + "     Value : " + MainActivity.StockObject.getString("latest")+ "€" + "   Change :" + MainActivity.StockObject.getString("percent"));
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
		
				viewGroup = (ViewGroup) findViewById(R.id.DiaGroup);
				
				try {
					dataset = createDatasetLineChart();
				} catch (JSONException e) {
					e.printStackTrace();
				} catch (InterruptedException e) {
					e.printStackTrace();
				} catch (ExecutionException e) {
					e.printStackTrace();
				}
				
				chartView = new ChartView(this);
				viewGroup.addView(chartView);
		
				try {
					dataset = createDatasetLineChart();
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (ExecutionException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
    			chartView.drawChart(ChartFactory.createLineChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));		
				
				
		final ListView listview = (ListView) findViewById(R.id.listNews);
		final ArrayList<String> list = new ArrayList<String>();
	    
		for (int i = 0; i < title.length; ++i) {
	      list.add(title[i]);
	    }
		
	    final StableArrayAdapter adapter = new StableArrayAdapter(this, android.R.layout.simple_list_item_1, list);
	    listview.setAdapter(adapter);

	    listview.setOnItemClickListener(new AdapterView.OnItemClickListener() {
	    
	    	/**
	    	 * @author Rickard Bremer
	    	 * This is an anonymous listener for the news list. Opens a webbrowser.
	    	 */
	    @Override  
	    public void onItemClick(AdapterView<?> parent, final View view, int position, long id) {
	    	Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(links[position]));
	    	startActivity(browserIntent);
	      }
	    });
		
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.main, menu);
		return true;
 	}
	
	/**
	 * @author Rickard Bremer
	 * Method onRadioButtonTime : These methods define what will happen when the radio buttons in the amrket view change 
	 * position
	 */
	public void onRadioButtonTime(View view) throws JSONException, InterruptedException, ExecutionException, ParseException {
	    // Is the button now checked?
	    boolean checked = ((RadioButton) view).isChecked();
	    
	    // Check which radio button was clicked
	    
	    switch(view.getId()) {
	        case R.id.onedaystock:
	            if (checked)	
	            	
	            	MainActivity.StockTime = System.currentTimeMillis() - 1 * 24 * 60 * 60 * 1000;
	            	this.stockArray = new DetailedStockThread().execute();
	      
	            		
	            		if(chart.equals("linechart")){
	            			System.out.println(this.stockArray.get().size());
	            			if(this.stockArray.get().size() > 0){
	            			dataset = createDatasetLineChart();
	    	    			chartView.drawChart(ChartFactory.createLineChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));	
	            			}
	            		}
	            		
	            		
	            		if(chart.equals("barchart")){
	            			System.out.println(this.stockArray.get().size());
	            			if(this.stockArray.get().size() > 0){
	            			dataset = createDatasetBarChart();
	            			chartView.drawChart(ChartFactory.createBarChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));    
	            			}
	            		}
	            		
	            		if(chart.equals("candlestick")){
	            			System.out.println(this.stockArray.get().size());
	            			if(this.stockArray.get().size() > 1){
	            			datasetCandle = createCandleStickDataset();
	            			chartView.drawChart(ChartFactory.createCandlestickChart(MainActivity.StockSymbol, "Time", "Value", datasetCandle, false));
	            			}
	            		}
	            			
	            		
	            		break;
	        
	        case R.id.oneweekstock:
	            if (checked)	
	            	 
	            	 MainActivity.StockTime = System.currentTimeMillis() - 7 * 24 * 60 * 60 * 1000;
	            	 this.stockArray = new DetailedStockThread().execute();
	           
	     				
	     		
	            	 	if(chart.equals("linechart")){
	            	 		dataset = createDatasetLineChart();
	            	 		chartView.drawChart(ChartFactory.createLineChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));	
	            	 	}
	            	 	
	            	 	if(chart.equals("barchart")){
	            	 		dataset = createDatasetBarChart();
	 	    				chartView.drawChart(ChartFactory.createBarChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));    
	            	 	}
	            	 	
	 	    			if(chart.equals("candlestick")){
		            		
	 	    				datasetCandle = createCandleStickDataset();
		            		chartView.drawChart(ChartFactory.createCandlestickChart(MainActivity.StockSymbol, "Time", "Value", datasetCandle, false));	
	 	    			}	
	 	    				
	 	    		break;
	            
	        case R.id.thirtydaysstock:
			
	        	long thirtyDays = 0;
			
			if (checked)
	            	thirtyDays = System.currentTimeMillis()/10;
			
	            	MainActivity.StockTime = thirtyDays - 3 * 24 * 60 * 60 * 1000;
	           
	            	this.stockArray = new DetailedStockThread().execute();
	            		
	            		if(chart.equals("linechart")){
	            			dataset = createDatasetLineChart();
	            			chartView.drawChart(ChartFactory.createLineChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));	
	            		}
	            		
	            		if(chart.equals("barchart")){
	            			dataset = createDatasetBarChart();
	            			chartView.drawChart(ChartFactory.createBarChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));    
	            		}
	            		
	            		if(chart.equals("candlestick")){
	            			
	            			datasetCandle = createCandleStickDataset();
	            			chartView.drawChart(ChartFactory.createCandlestickChart(MainActivity.StockSymbol, "Time", "Value", datasetCandle, false));
	            		}
	            		
	            	break;
	    }
	}
	
	/**
	 * @author Rickard Bremer
	 * Method onRadioButtonChart : These methods define what will happen when the radio buttons in the amrket view change 
	 * position
	 */
	public void onRadioButtonChart(View view) throws JSONException, InterruptedException, ExecutionException, ParseException {
	    // Is the button now checked?
	    boolean checked = ((RadioButton) view).isChecked();
	    
	    // Check which radio button was clicked
	    switch(view.getId()) {
	        case R.id.candlestick:
	        	
	            if (checked){
	            	datasetCandle = createCandleStickDataset();
	            	chartView.drawChart(ChartFactory.createCandlestickChart(MainActivity.StockSymbol, "Time", "Value", datasetCandle, false));	
            	    chart = "candlestick";
	            }
	            break;
	            
	        case R.id.linechart:
	        	
	            if (checked){
	            	dataset = createDatasetLineChart();
	    			chartView.drawChart(ChartFactory.createLineChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));	
	            	chart = "linechart";
	            }
	            break;
	            
	        case R.id.barchart:
	        	
	            if (checked){
	            	dataset = createDatasetBarChart();
	            	chartView.drawChart(ChartFactory.createBarChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));       
	            	chart = "barchart";
	            }
	            break;
	    }
	}
	
	private class StableArrayAdapter extends ArrayAdapter<String> {

	    HashMap<String, Integer> mIdMap = new HashMap<String, Integer>();

	    public StableArrayAdapter(Context context, int textViewResourceId, List<String> objects) {
	      super(context, textViewResourceId, objects);
	      for (int i = 0; i < objects.size(); ++i){
	        mIdMap.put(objects.get(i), i);
	      }
	    }

	    @Override
	    public long getItemId(int position) {
	      String item = getItem(position);
	      return mIdMap.get(item);
	    }

	    @Override
	    public boolean hasStableIds() {
	      return true;
	    }

	  }
	
	/**
	 * @author Rickard Bremer
	 * Build a dataset to display a graph.
	 */
	
 private  CategoryDataset createDatasetLineChart() throws JSONException, InterruptedException, ExecutionException {
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");

		String date = null; 
		java.sql.Timestamp timestamp = null;
  
		for(int i = 0; i < this.stockArray.get().size(); i++){                    
                    JSONObject JOBJ = new JSONObject(this.stockArray.get().get(i).toString());
                    String latest = JOBJ.getString("latest");
                    
                    if(latest.equals("-")){
                    	latest = "0";
                    }
                    
                    timestamp = new java.sql.Timestamp(Long.parseLong(JOBJ.getString("updated")));
                    
                    date = simpleDateFormat.format(timestamp);                             
                    dataset.addValue(Float.parseFloat(latest), "Value", date);               
        }
        
        return dataset;

    }
 	/**
	 * @author Rickard Bremer
	 * Build a dataset to display a graph.
	 */
	private  CategoryDataset createDatasetBarChart() throws JSONException, InterruptedException, ExecutionException {
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");

		String date = null; 
		java.sql.Timestamp timestamp1 = null;
		
		for(int i = 0; i < this.stockArray.get().size(); i++){                    
                    JSONObject JOBJ = new JSONObject(this.stockArray.get().get(i).toString());
                    String latest = JOBJ.getString("change");
                    
                    if(latest.equals("-")){
                    	latest = "0";
                    }
                    
                    timestamp1 = new java.sql.Timestamp(Long.parseLong(JOBJ.getString("updated")));
                    date = simpleDateFormat.format(timestamp1);     
                    dataset.addValue(Float.parseFloat(latest), "Value", date);               
                    
                    
		}
        
        return dataset;

    }
	
	/**
	 * @author Rickard Bremer
	 * Build a dataset to display a graph.
	 */
/*	private  DefaultHighLowDataset createCandleStickDataset() throws JSONException, InterruptedException, ExecutionException, ParseException {
		
		ArrayList<JSONObject> jobjList = new ArrayList<JSONObject>();
		ArrayList<JSONObject> newList = new ArrayList<JSONObject>();
		
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");
		
		ArrayList<Double> high = new ArrayList<Double>();
		ArrayList<Double> low = new ArrayList<Double>();
		ArrayList<Double> open = new ArrayList<Double>();
		ArrayList<Double> close = new ArrayList<Double>();
		ArrayList<Date> date = new ArrayList<Date>();
		ArrayList<Double> volume = new ArrayList<Double>();
		
		String dateTemp1 = null; 
		String dateTemp2 = null;
		
		java.sql.Timestamp timestamp1 = null;
		java.sql.Timestamp timestamp2 = null;
					
	//	System.out.println("stockArray : " + stockArray.get().size());
		
		for(int i = 0; i < MainActivity.MarketArray.get().size(); i++){
				
				JSONObject JOBJ = new JSONObject(MainActivity.MarketArray.get().get(i).toString());
                jobjList.add(JOBJ);
                System.out.println(jobjList.get(i));
			
			}
		
		System.out.println("jobjList : " + jobjList.size());
		for(int i = 0; i < jobjList.size(); i++){
			if(jobjList.get(i).getString("latest").equals("-")){
				System.out.println("Fel i värde");
			}
			else if(jobjList.get(i).getString("openVal").equals("-")){
				System.out.println("Fel i värde");
			}
			else{
				newList.add(jobjList.get(i));
			}
			
		}
		
		
		for(int i = 0; i < newList.size(); i++){
    	
    	ArrayList<JSONObject> tempJsonObjects = new ArrayList<JSONObject>();
    	
    	tempJsonObjects.add(newList.get(i));
    	
    		for(int j = i + 1; j < newList.size(); j++){
    			
    			System.out.println(newList.get(i).getString("updated"));
    			
    			timestamp1 = new java.sql.Timestamp(Long.parseLong(newList.get(i).getString("updated")));
    			timestamp2 = new java.sql.Timestamp(Long.parseLong(newList.get(j).getString("updated")));
    			
    		 dateTemp1 = simpleDateFormat.format(timestamp1);
    		 dateTemp2 = simpleDateFormat.format(timestamp2);
    	
    			System.out.println(dateTemp1 + " : " + dateTemp2 );
    			
    			if(dateTemp1.equals(dateTemp2)) {
    			//	System.out.println("dateTemp2 : " + dateTemp2);
    				tempJsonObjects.add(newList.get(j));
    		//		System.out.println("newList after remove 1 : " + newList.size());
    		//		System.out.println("tempJsonObj after add 1 : " + tempJsonObjects.size());
    				i++;
    			}
    	
    		
    		}
    	
    		// Sort and get opening value and closing value
    		//
    		
    		for(int k = 0; k < tempJsonObjects.size(); k++){
    			for (int l = 1; l < tempJsonObjects.size(); l++) {

    				long o1 = Long.parseLong(tempJsonObjects.get(k).getString("updated"));
    				long o2 = Long.parseLong(tempJsonObjects.get(l).getString("updated"));

//    				String ao1 = o1;
//    				String ao2 = o2;

    					if (o1 < o2) {
    						
    						JSONObject temp = new JSONObject(); 
    						temp = tempJsonObjects.get(k);
    						tempJsonObjects.set(k, tempJsonObjects.get(l));
    						tempJsonObjects.set(l, temp);
    					}
    			}
    		}
    		
    		open.add(Double.parseDouble(tempJsonObjects.get(0).getString("openVal")));
    		close.add(Double.parseDouble(tempJsonObjects.get(0).getString("latest")));

    		// Open and closing value is set
    		// ****
    		
    		
    		// Sort to get highest and lowest value
    		//
    		
    				for(int m = 0; m < tempJsonObjects.size(); m++){
    					for (int n = m + 1; n < tempJsonObjects.size(); n++) {

    						float o1 = Float.parseFloat(tempJsonObjects.get(m).getString("latest"));
    						float o2 = Float.parseFloat(tempJsonObjects.get(n).getString("latest"));
    							


    						if (o1 < o2) {
    								JSONObject temp = new JSONObject(); 
    								temp = tempJsonObjects.get(m);
    								tempJsonObjects.set(m, tempJsonObjects.get(n));
    								tempJsonObjects.set(n, temp);
    						}             
    					}
    		
    				}			
    				
    		high.add(Double.parseDouble(tempJsonObjects.get(0).getString("latest")));
        	low.add(Double.parseDouble(tempJsonObjects.get(tempJsonObjects.size()-1).getString("latest")));
        	
        	for(int ig = 0; ig < tempJsonObjects.size(); ig++){
        	
        		String temp12 = tempJsonObjects.get(ig).getString("latest");
        		System.out.println(temp12);
        	}
        	
    		//	Done with highest and lowest value.
    		//
    		
        	//
        	// Convert date
        	//
        	
        	String tempDate = simpleDateFormat.format(timestamp1);
        	System.out.println("TempDate : " + tempDate);
        	
        	// Add volume and date
        	//
        	
        	date.add(simpleDateFormat.parse(tempDate));
        	volume.add((double) 10);			
    
		}
    		
		
	
		//
		// Move arraylists to Double[] 
		
		double[] highd = new double[high.size()];
		double[] lowd = new double[low.size()];
		double[] closed = new double[close.size()];
		double[] opend = new double[open.size()];
		double[] volumed = new double[volume.size()];
		Date[] dated = new Date[date.size()];
		
		for(int i = 0; i < date.size(); i++){
			
			System.out.println(date.get(i)+ " : High ->: " + high.get(i) + ":  low -> : " + low.get(i) + " : open -> : " + open.get(i) + " :  close ->: " + close.get(i) + " : " + volume.get(i));
		
			highd[i] = high.get(i);
			lowd[i] = low.get(i);
			closed[i] = close.get(i);
			opend[i] = open.get(i);
			volumed[i] = volume.get(i);
			dated[i] = date.get(i);
		
		}
		
		DefaultHighLowDataset dataset = new DefaultHighLowDataset("Series 1 ", dated, highd, lowd, opend, closed, volumed);
		return dataset;
}*/
private  DefaultHighLowDataset createCandleStickDataset() throws JSONException, InterruptedException, ExecutionException, ParseException {
			
			ArrayList<JSONObject> jobjList = new ArrayList<JSONObject>();
			ArrayList<JSONObject> newList = new ArrayList<JSONObject>();
			
			SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");
			
			ArrayList<Double> high = new ArrayList<Double>();
			ArrayList<Double> low = new ArrayList<Double>();
			ArrayList<Double> open = new ArrayList<Double>();
			ArrayList<Double> close = new ArrayList<Double>();
			ArrayList<Date> date = new ArrayList<Date>();
			ArrayList<Double> volume = new ArrayList<Double>();
			
			String dateTemp1 = null; 
			String dateTemp2 = null;
			
			java.sql.Timestamp timestamp1 = null;
			java.sql.Timestamp timestamp2 = null;
						
			System.out.println("stockArray : " + stockArray.get().size());
			
			for(int i = 0; i < stockArray.get().size(); i++){
					
					JSONObject JOBJ = new JSONObject(stockArray.get().get(i).toString());
                    jobjList.add(JOBJ);
                    System.out.println(jobjList.get(i));
				
				}
			
			System.out.println("jobjList : " + jobjList.size());
			
			for(int i = 0; i < jobjList.size(); i++){
				if(jobjList.get(i).getString("latest").equals("-")){
					System.out.println("Fel i värde");
				}
				else if(jobjList.get(i).getString("openVal").equals("-")){
					System.out.println("Fel i värde");
				}
				else{
					newList.add(jobjList.get(i));
				}
				
			}
			
			for(int i = 0; i < newList.size(); i++){
        	
        	ArrayList<JSONObject> tempJsonObjects = new ArrayList<JSONObject>();
        	
        	tempJsonObjects.add(newList.get(i));
        	
        		for(int j = i + 1; j < newList.size(); j++){
        			
        			System.out.println(newList.get(i).getString("updated"));
        			
        			timestamp1 = new java.sql.Timestamp(Long.parseLong(newList.get(i).getString("updated")));
        			timestamp2 = new java.sql.Timestamp(Long.parseLong(newList.get(j).getString("updated")));
        			
        		 dateTemp1 = simpleDateFormat.format(timestamp1);
        		 dateTemp2 = simpleDateFormat.format(timestamp2);
        	
        			System.out.println(dateTemp1 + " : " + dateTemp2 );
        			
        			if(dateTemp1.equals(dateTemp2)) {
        			//	System.out.println("dateTemp2 : " + dateTemp2);
        				tempJsonObjects.add(newList.get(j));
        		//		System.out.println("newList after remove 1 : " + jobjList.size());
        		//		System.out.println("tempJsonObj after add 1 : " + tempJsonObjects.size());
        				i++;
        			}
        	
        		
        		}
        	
        		// Sort and get opening value and closing value
        		//
        		
        		for(int k = 0; k < tempJsonObjects.size(); k++){
        			for (int l = 1; l < tempJsonObjects.size(); l++) {

        				long o1 = Long.parseLong(tempJsonObjects.get(k).getString("updated"));
        				long o2 = Long.parseLong(tempJsonObjects.get(l).getString("updated"));

//        				String ao1 = o1;
//        				String ao2 = o2;

        					if (o1 < o2) {
        						
        						JSONObject temp = new JSONObject(); 
        						temp = tempJsonObjects.get(k);
        						tempJsonObjects.set(k, tempJsonObjects.get(l));
        						tempJsonObjects.set(l, temp);
        					}
        			}
        		}
			open.add(Double.parseDouble(tempJsonObjects.get(0).getString("openVal")));
    		close.add(Double.parseDouble(tempJsonObjects.get(0).getString("latest")));
        		
//        		 String tempOpenVal = tempJsonObjects.get(0).getString("openVal");
//        		 String tempClose = tempJsonObjects.get(0).getString("latest");
//        		 
//        		 if(tempOpenVal.equals("-")){
//        			 tempOpenVal = "0";
//        		 }
//        		 
//        		 if(tempClose.equals("-")){
//        			 tempClose = "0";
//        		 }
//        		 
//        		open.add(Double.parseDouble(tempOpenVal));
//        		close.add(Double.parseDouble(tempClose));

        		// Open and closing value is set
        		// ****
        		
        		
        		// Sort to get highest and lowest value
        		//
        		
//        				for(int m = 0; m < tempJsonObjects.size(); m++){
//        					for (int n = m + 1; n < tempJsonObjects.size(); n++) {
//
//        						String o1 = tempJsonObjects.get(m).getString("latest");
//        						String o2 = tempJsonObjects.get(n).getString("latest");
//        							
//        						if(o1.equals("-")){
//        	                       o1 = "0";
//        	                    }
//        						
//        						if(o2.equals("-")){
//        	                    	o2 = "0";
//        	                    }
//        						
//        						Double fo1 = Double.parseDouble(o1);
//        						Double fo2 = Double.parseDouble(o2);
//
//        						if (fo1 < fo2) {
//        								JSONObject temp = new JSONObject(); 
//        								temp = tempJsonObjects.get(m);
//        								tempJsonObjects.set(m, tempJsonObjects.get(n));
//        								tempJsonObjects.set(n, temp);
//        						}             
//        					}
//        		
//        				}	
						
						
    				for(int m = 0; m < tempJsonObjects.size(); m++){
    					for (int n = m + 1; n < tempJsonObjects.size(); n++) {

    						float o1 = Float.parseFloat(tempJsonObjects.get(m).getString("latest"));
    						float o2 = Float.parseFloat(tempJsonObjects.get(n).getString("latest"));
    							


    						if (o1 < o2) {
    								JSONObject temp = new JSONObject(); 
    								temp = tempJsonObjects.get(m);
    								tempJsonObjects.set(m, tempJsonObjects.get(n));
    								tempJsonObjects.set(n, temp);
    						}             
    					}
    		
    				}			
        				
        				high.add(Double.parseDouble(tempJsonObjects.get(0).getString("latest")));
        	            low.add(Double.parseDouble(tempJsonObjects.get(tempJsonObjects.size()-1).getString("latest")));		
        				
//        				String tempHigh = tempJsonObjects.get(0).getString("latest");
//        				String tempLow = tempJsonObjects.get(tempJsonObjects.size()-1).getString("latest");
//        						
//        				if(tempHigh.equals("-")){
//        					tempHigh = "0";
//        				}
//        				if(tempLow.equals("-")){
//        					tempLow = "0";
//        				}
//        				
//        		high.add(Double.parseDouble(tempHigh));
//        		
//            	low.add(Double.parseDouble(tempLow));
            	
            	for(int ig = 0; ig < tempJsonObjects.size(); ig++){
            	
            		String temp12 = tempJsonObjects.get(ig).getString("latest");
            		System.out.println(temp12);
            	}
            	
        		//	Done with highest and lowest value.
        		//
        		
            	//
            	// Convert date
            	//
            	
            	String tempDate = simpleDateFormat.format(timestamp1);
            	System.out.println("TempDate : " + tempDate);
            	
            	// Add volume and date
            	//
            	
            	date.add(simpleDateFormat.parse(tempDate));
            	volume.add((double) 10);			
        
			}
        		
			
		
			//
			// Move arraylists to Double[] 
			
			double[] highd = new double[high.size()];
			double[] lowd = new double[low.size()];
			double[] closed = new double[close.size()];
			double[] opend = new double[open.size()];
			double[] volumed = new double[volume.size()];
			Date[] dated = new Date[date.size()];
			
			for(int i = 0; i < date.size(); i++){
				
				System.out.println(date.get(i)+ " : High ->: " + high.get(i) + ":  low -> : " + low.get(i) + " : open -> : " + open.get(i) + " :  close ->: " + close.get(i) + " : " + volume.get(i));
			
				highd[i] = high.get(i);
				lowd[i] = low.get(i);
				opend[i] = open.get(i);
				volumed[i] = volume.get(i);
				dated[i] = date.get(i);
			
			}
			
			DefaultHighLowDataset dataset = new DefaultHighLowDataset("Series 1 ", dated, highd, lowd, opend, closed, volumed);
			return dataset;
	}
		/**
		 * @author Justin Inácio
		 * Method which is executed when the specified button is pressed. 
		 * Used to add the stock (being shown in detailed view) to the user's portfolio.
		 * @throws JSONException
		 */

	public void addToPortfolio(View view) throws JSONException{
		if (MainActivity.globalArrayTest.contains(MainActivity.StockObject.getString("name"))) {
			Context context = getApplicationContext();
			CharSequence text = "Portfolio already contains "+ MainActivity.StockObject.getString("name") + "!";
			int duration = Toast.LENGTH_SHORT;

			Toast toast = Toast.makeText(context, text, duration);
			toast.show();
		} else {
			Context context = getApplicationContext();
			CharSequence text = MainActivity.StockObject.getString("name")+ " added to portfolio!";
			int duration = Toast.LENGTH_SHORT;
			MainActivity.globalArrayTest.add(MainActivity.StockObject.getString("name"));
			Toast toast = Toast.makeText(context, text, duration);
			toast.show();
		}
		
	}

} 
