package com.example.mercbisandroid;

import java.util.ArrayList;
import java.util.concurrent.ExecutionException;

import org.afree.chart.ChartFactory;
import org.afree.chart.plot.PlotOrientation;
import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.RadioButton;

//import android.view.LayoutInflater;

/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */

public class MarketFragment extends Fragment {
	
	AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> stockArray;
	CategoryDataset dataset = createDataset();
	ViewGroup viewGroup = null;
	ChartView chartView;
	LinearLayout linearLayout;
	
	public MarketFragment() {
		// Required empty public constructor
	}
	
public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);                
}


@Override
public void onActivityCreated(Bundle savedInstanceState) {

	//	stockArray = new DetailedStockThread().execute();	
		
		
			
		
			
	super.onActivityCreated(savedInstanceState);
	return;
		
		       
		
	}
  
@Override
public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		
		
    	View view = inflater.inflate(R.layout.fragment_market, container, false);
    	
    	viewGroup = (ViewGroup) view.findViewById(R.id.marketGraph); 
    	
    	chartView = new ChartView(getActivity());
    	
    	chartView.drawChart(ChartFactory.createBarChart("Hej","Time", "Value", createDataset(), PlotOrientation.VERTICAL, true, true, false));
    	
    	viewGroup.addView(chartView);
    	
return view;
    	
}

    
 
    private static CategoryDataset createDataset() {

        // row keys...
        String series1 = "First";
        String series2 = "Second";
        String series3 = "Third";

        // column keys...
        String category1 = "Category 1";
        String category2 = "Category 2";
        String category3 = "Category 3";
        String category4 = "Category 4";
        String category5 = "Category 5";

        // create the dataset...
        DefaultCategoryDataset dataset = new DefaultCategoryDataset();

        dataset.addValue(1.0, series1, category1);
        dataset.addValue(4.0, series1, category2);
        dataset.addValue(3.0, series1, category3);
        dataset.addValue(5.0, series1, category4);
        dataset.addValue(5.0, series1, category5);

        dataset.addValue(5.0, series2, category1);
        dataset.addValue(7.0, series2, category2);
        dataset.addValue(6.0, series2, category3);
        dataset.addValue(8.0, series2, category4);
        dataset.addValue(4.0, series2, category5);

        dataset.addValue(4.0, series3, category1);
        dataset.addValue(3.0, series3, category2);
        dataset.addValue(2.0, series3, category3);
        dataset.addValue(3.0, series3, category4);
        dataset.addValue(6.0, series3, category5);

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
   
   /* private  DefaultHighLowDataset createCandleStickDataset() throws JSONException, InterruptedException, ExecutionException, ParseException {
		
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
}

    




