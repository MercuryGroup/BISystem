package com.example.mercbisandroid;

//import com.example.list_fruit.R;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import org.json.JSONException;
import org.json.JSONObject;

import android.support.v4.app.Fragment;
import android.support.v4.app.ListFragment;

import android.os.AsyncTask;
//import android.app.ListFragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Toast;
import android.widget.TextView;

/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */
public class MarketFragment extends Fragment {
	
//	JSONObject JOBJ = new JSONObject();
//	String[] STOCKS;
 //	static final String[] STOCKS = new String[] { "Stock1", "Stock2", "Stock3",
//		"Stock4", "Stock5", "Stock6", "Stock7", "Stock8",
//		"Stock9", "Stock10", "Stock11", "Stock12", "Stock13" };

//AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> STOCKS = new StockThread().execute();
	 
	public MarketFragment() {
		// Required empty public constructor
	}

	/*@Override
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
	*/

/*public void onCreate(Bundle savedInstanceState) {
			super.onCreate(savedInstanceState);
			
			
			
	 
		}*/
	
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		// Inflate the layout for this fragment
		return inflater.inflate(R.layout.fragment_market, container, false);
	}
	 
		
	}


