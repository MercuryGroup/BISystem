package com.example.mercbisandroid;

import java.util.List;
import android.os.Bundle;
import android.support.v4.app.ListFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ListView;


/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */
public class PortfolioFragment extends ListFragment {
	
	ListView l;
	ArrayAdapter<String> adapter;
	List<String> list;
	

	public PortfolioFragment() {
		// Required empty public constructor
	}
	
	
	public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);                
        
}
	

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		// Inflate the layout for this fragment
		list = MainActivity.globalArrayTest;
		
		adapter = new ArrayAdapter<String>(getActivity(), android.R.layout.simple_list_item_1, list);
		View v = inflater.inflate(R.layout.fragment_portfolio, container, false);
		l = (ListView) v.findViewById(android.R.id.list);
		l.setAdapter(adapter);
	
		return v;

	}
	
	
	public void onActivityCreated(Bundle savedInstanceState) {
		
		 super.onActivityCreated(savedInstanceState);
		 list = MainActivity.globalArrayTest;
		 l = getListView();
		 
		
	}
}