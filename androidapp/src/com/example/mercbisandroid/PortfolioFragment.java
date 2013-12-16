package com.example.mercbisandroid;

import android.os.Bundle;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.support.v4.app.ListFragment;


/**
 * A simple {@link android.support.v4.app.Fragment} subclass.
 * 
 */
public class PortfolioFragment extends ListFragment {
	
	// Rickard Bremer
	String[] values = new String[] { "Mercury 0","Mercury 1",
	        "Mercury 2", "Mercury 3", "Mercury 4", "Mercury 5", "Mercury 6",
	        "Mercury 7", "Mercury 8", "Mercury 9", "Mercury 10", "Mercury 11", "Mercury 12",
	        "Mercury 13", "Mercury 14", "Mercury 15", "Mercury 16", "Mercury 17", "Mercury 18",
	        "Mercury 19", "Mercury 20"};

	public PortfolioFragment() {
		// Required empty public constructor
	}
	
	
	public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);                

}
	



	
	@Override
public void onActivityCreated(Bundle savedInstanceState) {
		// Rickard Bremer
		//return inflater.inflate(R.layout.fragment_portfolio, container, false);
		 setListAdapter(new ArrayAdapter<String>(getActivity(), R.layout.list_portfolio,values));
        
        
        ListView listView = getListView(); //EX:
        registerForContextMenu(listView);
        super.onActivityCreated(savedInstanceState);
        return;
 
	}
	
}