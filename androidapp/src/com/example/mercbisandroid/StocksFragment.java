package com.example.mercbisandroid;

import java.util.ArrayList;
import java.util.concurrent.ExecutionException;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.ListFragment;
import android.widget.ArrayAdapter;
import android.widget.ListView;

/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */
public class StocksFragment extends ListFragment {
	
	JSONObject JOBJ = new JSONObject();
	String[] STOCKS;

	public StocksFragment() {
		// Required empty public constructor
	}
	
public void onActivityCreated(Bundle savedInstanceState) {
		
		AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> execute = new StockThread().execute();
		
		
		try {
			STOCKS = new String[execute.get().size()];
		} catch (InterruptedException e1) {
			e1.printStackTrace();
		} catch (ExecutionException e1) {
			e1.printStackTrace();
		}
		
		
		
	    try {
	    	for (int i = 0; i < execute.get().size(); i++){
	    	    String JsonLine = execute.get().get(i).toString();
	    		JSONObject JOBJ;
				try {
					
					JOBJ = new JSONObject(JsonLine);
					STOCKS[i] = JOBJ.getString("symbol");
					System.out.println("Symbol : " + JOBJ.getString("symbol"));
					
				} catch (JSONException e) {
				
					e.printStackTrace();
				}
	    		
	    	}
	    	
		} catch (InterruptedException e) {
			e.printStackTrace();
		} catch (ExecutionException e) {
			e.printStackTrace();
		}
		 
			setListAdapter(new ArrayAdapter<String>(getActivity(), R.layout.list_stocks,STOCKS));
		    ListView listView = getListView(); //EX: 
		    listView.setTextFilterEnabled(true);
		    registerForContextMenu(listView);
		    super.onActivityCreated(savedInstanceState);
		    return;
		
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);		
 
	}
 
	
}

//	public View onCreateView(LayoutInflater inflater, ViewGroup container,
//			Bundle savedInstanceState) {
		// Inflate the layout for this fragment
//		return inflater.inflate(R.layout.fragment_stocks, container, false);
//	}


