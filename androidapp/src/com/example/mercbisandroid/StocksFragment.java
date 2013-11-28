package com.example.mercbisandroid;

import java.util.ArrayList;
import java.util.concurrent.ExecutionException;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.ListFragment;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;

/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */
public class StocksFragment extends ListFragment implements OnItemClickListener{
	
	JSONObject JOBJ = new JSONObject();
	String[] STOCKS;
	public	ArrayList<JSONObject> STOCKLIST = new ArrayList<JSONObject>();
	
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
	    		//JSONObject JOBJ;
				try {
					
					JOBJ = new JSONObject(JsonLine);
					STOCKLIST.add(JOBJ);
					//STOCKS[i] = JOBJ.getString("symbol");
					STOCKS[i] = STOCKLIST.get(i).getString("symbol");
				//	System.out.println("Symbol : " + STOCKS[i]);
					
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
		    
		    listView.setOnItemClickListener(new OnItemClickListener() {
		        public void onItemClick(AdapterView<?> parent, View view,int position, long id) {
		                // When clicked, show a toast with the TextView text
		               System.out.println(STOCKLIST.get(position));
		            }
		        });
		    
		    
		    registerForContextMenu(listView);
		    super.onActivityCreated(savedInstanceState);
		    return;
		
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);		
 
	}

	@Override
	public void onItemClick(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
		System.out.println("Hello World");
		
	}
 
	
}

//	public View onCreateView(LayoutInflater inflater, ViewGroup container,
//			Bundle savedInstanceState) {
		// Inflate the layout for this fragment
//		return inflater.inflate(R.layout.fragment_stocks, container, false);
//	}


