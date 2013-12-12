package com.example.mercbisandroid;

import java.util.concurrent.ExecutionException;
import org.afree.data.category.CategoryDataset;
import org.afree.data.category.DefaultCategoryDataset;
import org.json.JSONException;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.os.Bundle;

/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */

public class MarketFragment extends Fragment {
	

	public MarketFragment() {
		// Required empty public constructor
	}

//	@Override
//	public void onActivityCreated(Bundle savedInstanceState) {
//		
////		ViewGroup viewGroup = (ViewGroup) getView().findViewById(R.id.DiaGroup);
////		
////		CategoryDataset dataset = null;
////		try {
////			dataset = createDataset();
////		} catch (JSONException e) {		
////			// TODO Auto-generated catch block
////			e.printStackTrace();
////		} catch (InterruptedException e) {
////			// TODO Auto-generated catch block
////			e.printStackTrace();
////		} catch (ExecutionException e) {
////			// TODO Auto-generated catch block
////			e.printStackTrace();
////		}
////		
////		
////		ChartView chartView = (ChartView) getView();
////		
////		chartView.drawChart(ChartFactory.createBarChart(MainActivity.StockSymbol,"Time", "Value", dataset, PlotOrientation.VERTICAL, true, true, false));       
////		
////		viewGroup.addView(chartView);
//	}
  
    @Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		// Inflate the layout for this fragment
    	
		return inflater.inflate(R.layout.fragment_market, container, false);
	}
}
    
//    private  CategoryDataset createDataset() throws JSONException, InterruptedException, ExecutionException {
//		DefaultCategoryDataset dataset = new DefaultCategoryDataset();
//        
//		// row keys...
//
//        // column keys...
//        
////        // create the dataset...
////        for(int i = 0; i < stockArray.get().size(); i++){
////                    
////                    JSONObject JOBJ = new JSONObject(stockArray.get().get(i).toString());
////                    
////                    
////                    String latest = JOBJ.getString("latest");
////                    String updated = JOBJ.getString("updated");
////                    System.out.println("latest :" + latest + " # "+" updated : " + updated);
////                   
////                    dataset.addValue(Float.parseFloat(latest), "Value", " ");
////        }
////        
//        return dataset;
//
//    }
//
//
//
//		
//	}


