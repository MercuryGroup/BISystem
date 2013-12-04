package com.example.mercbisandroid;


import android.os.Bundle;
import android.view.ViewGroup;

import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;

/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */
public class StockGraphFragment extends Fragment {

	public StockGraphFragment() {
		
		// Required empty public constructor
	}
	
	
	/*@Override
	public void onCreate(Bundle savedInstanceState) {
	    super.onCreate(savedInstanceState);
	    
	}
	*/

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		
		return inflater.inflate(R.layout.fragment_stockgraph, container, false);
		
	}
}