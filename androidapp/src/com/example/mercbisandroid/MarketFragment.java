package com.example.mercbisandroid;

//import com.example.list_fruit.R;

import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ExecutionException;

import org.json.JSONObject;

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
public class MarketFragment extends ListFragment {
	
	static final String[] STOCKS = new String[] { "Stock1", "Stock2", "Stock3",
		"Stock4", "Stock5", "Stock6", "Stock7", "Stock8",
		"Stock9", "Stock10", "Stock11", "Stock12", "Stock13" };

//AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> STOCKS = new StockThread().execute();
	 
	public MarketFragment() {
		// Required empty public constructor
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
	
		
	//	@SuppressWarnings("unchecked")
		System.out.println("test2");
		
	 //  ArrayList<ArrayList> AL = new ArrayList();
	   
		
		//System.out.println("Lista " + Lista.get().);
		
	//	JSONObject JOBJ = new JSONObject();
		
		//JOBJ.getJSONObject(Lista.get(1));
		
		
		
		//ArrayList<Object> Test = STOCKS.doInBackground(null);
		
		
		     
			setListAdapter(new ArrayAdapter<String>(getActivity(), R.layout.list_stocks,STOCKS));
		    ListView listView = getListView(); //EX: 
		    listView.setTextFilterEnabled(true);
		    registerForContextMenu(listView);
		    super.onActivityCreated(savedInstanceState);
		
	}
	
	//@Override
	
//	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		//public void onCreate(Bundle savedInstanceState) {
	//		super.onCreate(savedInstanceState);
	 
			// no more this
			// setContentView(R.layout.list_fruit);
	 
			//setListAdapter(new ArrayAdapter<String>(getActivity(), R.layout.list_fruit,FRUITS));
	 
			//ListView listView = getListView();
			//listView.setTextFilterEnabled(true);
	 
			//listView.setOnItemClickListener(new OnItemClickListener() {
			//	public void onItemClick(AdapterView<?> parent, View view,
			//			int position, long id) {
				    // When clicked, show a toast with the TextView text
			//	    Toast.makeText(getActivity().getApplicationContext(),((TextView) view).getText(), Toast.LENGTH_SHORT).show();
			//	}
		//	});
		//	return 
	 
		//}
	 
		
	}


