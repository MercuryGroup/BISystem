package com.example.mercbisandroid;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.ExecutionException;

import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;
import org.afree.data.xy.DefaultHighLowDataset;
import org.json.JSONException;
import org.json.JSONObject;

//import android.app.Fragment;
import android.net.ParseException;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;

/**
 * @author Rickard Bremer
 * This class displays the market data overview in the Market fragment.
 * 
 */


public class MarketFragment extends Fragment {
	
	AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> stockArray;
	static CategoryDataset dataset;
	static DefaultHighLowDataset datacandle; 
	ViewGroup viewGroup = null;
	static ChartView chartView;
	LinearLayout linearLayout;
	
	
public MarketFragment() {
		// Required empty public constructor
	}

	
public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
}


@Override
public void onActivityCreated(Bundle savedInstanceState) {

	
		
			
	super.onActivityCreated(savedInstanceState);
	return;
		
	}


/**
 * @author Rickard Bremer
 * Display the graph in the fragment
 * 
 */
@Override
public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		
		View view = inflater.inflate(R.layout.fragment_market, container, false);
		

    	viewGroup = (ViewGroup) view.findViewById(R.id.marketGraph); 
    	
    	chartView = new ChartView(getActivity());
    	
    //	chartView.drawChart(ChartFactory.createBarChart("Hej","Time", "Value", createDataset(), PlotOrientation.VERTICAL, true, true, false));
    	
    	viewGroup.addView(chartView);
    	
    	return view;
    	
}
     
/**
 * @author Rickard Bremer
 * Build a dataset to display a graph.
 * 
 */
 static CategoryDataset createDatasetBarChart() throws JSONException, InterruptedException, ExecutionException {
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");
		String date = null; 
		java.sql.Timestamp timestamp = null;
		
		
		for(int i = 0; i < MainActivity.MarketArray.get().size(); i++){                    
                    JSONObject JOBJ = new JSONObject(MainActivity.MarketArray.get().get(i).toString());
                    String latest = JOBJ.getString("change");
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
 
    static CategoryDataset createDatasetLineChart() throws JSONException, InterruptedException, ExecutionException {
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy");
		String date = null; 
		java.sql.Timestamp timestamp = null;
		
		
		for(int i = 0; i < MainActivity.MarketArray.get().size(); i++){                    
                    JSONObject JOBJ = new JSONObject(MainActivity.MarketArray.get().get(i).toString());
                    String latest = JOBJ.getString("latest");
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
   
static DefaultHighLowDataset createCandleStickDataset() throws JSONException, InterruptedException, ExecutionException, ParseException, java.text.ParseException {
		
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
					
	//	System.out.println("stockArray : " + stockArray.get().size());
		
		for(int i = 0; i < MainActivity.MarketArray.get().size(); i++){
				
				JSONObject JOBJ = new JSONObject(MainActivity.MarketArray.get().get(i).toString());
                jobjList.add(JOBJ);
      //          System.out.println(jobjList.get(i));
			
			}
		
	//	System.out.println("jobjList : " + jobjList.size());
		
		for(int i = 0; i < jobjList.size(); i++){
    	
    	ArrayList<JSONObject> tempJsonObjects = new ArrayList<JSONObject>();
    	
    	tempJsonObjects.add(jobjList.get(i));
    	
    		for(int j = i + 1; j < jobjList.size(); j++){
    			
    		//	System.out.println(jobjList.get(i).getString("updated"));
    			
    			timestamp1 = new java.sql.Timestamp(Long.parseLong(jobjList.get(i).getString("updated")));
    			timestamp2 = new java.sql.Timestamp(Long.parseLong(jobjList.get(j).getString("updated")));
    			
    		 dateTemp1 = simpleDateFormat.format(timestamp1);
    		 dateTemp2 = simpleDateFormat.format(timestamp2);
    	
    	//		System.out.println(dateTemp1 + " : " + dateTemp2 );
    			
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
        		//System.out.println(temp12);
        	}
        	
    		//	Done with highest and lowest value.
    		//
    		
        	//
        	// Convert date
        	//
        	
        	String tempDate = simpleDateFormat.format(timestamp1);
        	//System.out.println("TempDate : " + tempDate);
        	
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
			
		//	System.out.println(date.get(i)+ " : High ->: " + high.get(i) + ":  low -> : " + low.get(i) + " : open -> : " + open.get(i) + " :  close ->: " + close.get(i) + " : " + volume.get(i));
		
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

    




