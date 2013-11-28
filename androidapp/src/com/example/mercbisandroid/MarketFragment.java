package com.example.mercbisandroid;

//import com.example.list_fruit.R;


import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentTabHost;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.ActionBar;
import android.app.FragmentTransaction;
//import android.app.ListFragment;
import android.os.Bundle;

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
	
////	ActionBar actionBar;
//	
////	public void onCreate(Bundle arg0) {
////		super.onCreate(arg0);
////	public View onCreateView(LayoutInflater inflater, ViewGroup container,
////			Bundle savedInstanceState) {
//		// Inflate the layout for this fragment
//		
//	//	actionBar = getActionBar();
//		actionBar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);
//
//		ActionBar.Tab stocks = actionBar.newTab();
//		stocks.setText("Stocks");
//		stocks.setTabListener(this);
//
//		ActionBar.Tab market = actionBar.newTab();
//		market.setText("Market");
//		market.setTabListener(this);
//
//		ActionBar.Tab portfolio = actionBar.newTab();
//		portfolio.setText("Portfolio");
//		portfolio.setTabListener(this);
//
//		actionBar.addTab(stocks);
//		actionBar.addTab(market);
//		actionBar.addTab(portfolio);
//	//	return inflater.inflate(R.layout.fragment_market, container, false);
//	
//	}
	
//	private FragmentTabHost mTabHost;

  
    @Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		// Inflate the layout for this fragment
		return inflater.inflate(R.layout.fragment_market, container, false);
	}
//	public void onCreate(Bundle savedInstanceState) {
//        super.onCreate(savedInstanceState);

//        setContentView(R.layout.market_tabs);
//        mTabHost = (FragmentTabHost)findViewById(android.R.id.tabhost);
//        mTabHost.setup(this, getSupportFragmentManager(), R.id.realtabcontent);
//
//        mTabHost.addTab(mTabHost.newTabSpec("simple").setIndicator("Simple"),
//                FragmentStackSupport.CountingFragment.class, null);
//        mTabHost.addTab(mTabHost.newTabSpec("contacts").setIndicator("Contacts"),
//                LoaderCursorSupport.CursorLoaderListFragment.class, null);
//        mTabHost.addTab(mTabHost.newTabSpec("custom").setIndicator("Custom"),
//                LoaderCustomSupport.AppListFragment.class, null);
//        mTabHost.addTab(mTabHost.newTabSpec("throttle").setIndicator("Throttle"),
//                LoaderThrottleSupport.ThrottledLoaderListFragment.class, null);
//    }

		
	}


