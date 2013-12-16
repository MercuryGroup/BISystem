
package com.example.mercbisandroid;
import java.security.Timestamp;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ExecutionException;

/*import org.afree.chart.ChartFactory;
import org.afree.chart.plot.PlotOrientation;
import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;*/

import org.afree.chart.ChartFactory;
import org.afree.chart.plot.PlotOrientation;
import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;
import org.afree.data.xy.DefaultHighLowDataset;
import org.json.JSONException;
import org.json.JSONObject;

import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;

import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.RadioButton;
import android.widget.TextView;

/**
 * @author Rickard Bremer
 *
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
					stockInfo.setText(MainActivity.StockObject.getString("name"));
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
	      for (int i = 0; i < objects.size(); ++i) {
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
	
	
 private  CategoryDataset createDatasetLineChart() throws JSONException, InterruptedException, ExecutionException {
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
  
		for(int i = 0; i < this.stockArray.get().size(); i++){                    
                    JSONObject JOBJ = new JSONObject(this.stockArray.get().get(i).toString());
                    String latest = JOBJ.getString("latest");
                    String updated = JOBJ.getString("updated");          
                    dataset.addValue(Float.parseFloat(latest), "Value", updated);               
        }
        
        return dataset;

    }
	
	private  CategoryDataset createDatasetBarChart() throws JSONException, InterruptedException, ExecutionException {
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
  
		for(int i = 0; i < this.stockArray.get().size(); i++){                    
                    JSONObject JOBJ = new JSONObject(this.stockArray.get().get(i).toString());
                    String latest = JOBJ.getString("change");
                    String updated = JOBJ.getString("updated");          
                    dataset.addValue(Float.parseFloat(latest), "Value", updated);               
        }
        
        return dataset;

    }

private  DefaultHighLowDataset createCandleStickDataset() throws JSONException, InterruptedException, ExecutionException, ParseException {
			
			ArrayList<JSONObject> jobjList = new ArrayList<JSONObject>();
			
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
        	
        	ArrayList<JSONObject> tempJsonObjects = new ArrayList<JSONObject>();
        	
        	tempJsonObjects.add(jobjList.get(i));
        	
        		for(int j = i + 1; j < jobjList.size(); j++){
        			
        			System.out.println(jobjList.get(i).getString("updated"));
        			
        			timestamp1 = new java.sql.Timestamp(Long.parseLong(jobjList.get(i).getString("updated")));
        			timestamp2 = new java.sql.Timestamp(Long.parseLong(jobjList.get(j).getString("updated")));
        			
        		 dateTemp1 = simpleDateFormat.format(timestamp1);
        		 dateTemp2 = simpleDateFormat.format(timestamp2);
        	
        			System.out.println(dateTemp1 + " : " + dateTemp2 );
        			
        			if(dateTemp1.equals(dateTemp2)) {
        			//	System.out.println("dateTemp2 : " + dateTemp2);
        				tempJsonObjects.add(jobjList.get(j));
        		//		System.out.println("jObjList after remove 1 : " + jobjList.size());
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
	}
} 
