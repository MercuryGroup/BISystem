package com.example.mercbisandroid;

import java.util.ArrayList;

import android.app.ActionBar;
import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.FragmentTransaction;
import android.app.SearchManager;
import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.Menu;
import android.widget.SearchView;

public class MainActivity extends FragmentActivity implements TabListener {
	
	public static AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> stockArray;
	
	
	private ArrayList<String> globalArrayTest = new ArrayList<String>();
	
	ActionBar actionBar;
	ViewPager viewPager;

	@Override
	protected void onCreate(Bundle arg0) {
		super.onCreate(arg0);
		setContentView(R.layout.activity_main);

		//Rickard Bremer
		checkInternetConnection();	
		
		viewPager=(ViewPager) this.findViewById(R.id.tabs);
		viewPager.setAdapter(new adapter(getSupportFragmentManager()));
		viewPager.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {
			
			@Override
			public void onPageSelected(int arg0) {
				// TODO Auto-generated method stub
				actionBar.setSelectedNavigationItem(arg0);
			}
			
			@Override
			public void onPageScrolled(int arg0, float arg1, int arg2) {
				// TODO Auto-generated method stub
				
			}
			
			@Override
			public void onPageScrollStateChanged(int arg0) {
				// TODO Auto-generated method stub
				
			}
		});
		
		actionBar = getActionBar();
		actionBar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);

		ActionBar.Tab stocks = actionBar.newTab();
		stocks.setText("Stocks");
		stocks.setTabListener(this);

		ActionBar.Tab market = actionBar.newTab();
		market.setText("Market");
		market.setTabListener(this);

		ActionBar.Tab portfolio = actionBar.newTab();
		portfolio.setText("Portfolio");
		portfolio.setTabListener(this);

		actionBar.addTab(stocks);
		actionBar.addTab(market);
		actionBar.addTab(portfolio);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.main, menu);
		
	    SearchManager searchManager =
	            (SearchManager) getSystemService(Context.SEARCH_SERVICE);
	     SearchView searchView =
	             (SearchView) menu.findItem(R.id.search).getActionView();
	     searchView.setSearchableInfo(
	             searchManager.getSearchableInfo(getComponentName()));
		
		
		return true;
	}

	public void addToArray(String s) {
		globalArrayTest.add(s);
	}
	
	public ArrayList<String> showArray() {
		return globalArrayTest;
	}
	
	
	
	@Override
	public void onTabReselected(Tab arg0, FragmentTransaction arg1) {
		// TODO Auto-generated method stub

	}

	@Override
	public void onTabSelected(Tab tab, FragmentTransaction ft) {
		// TODO Auto-generated method stub
		viewPager.setCurrentItem(tab.getPosition());
	}

	@Override
	public void onTabUnselected(Tab tab, FragmentTransaction ft) {
		// TODO Auto-generated method stub

	}

	class adapter extends FragmentPagerAdapter {

		public adapter(FragmentManager fm) {
			super(fm);
			// TODO Auto-generated constructor stub
		}

		public Fragment getItem(int arg0) {
			Fragment fragment = null;
			if (arg0 == 0) {
				fragment = new StocksFragment();
			}
			if (arg0 == 1) {
				fragment = new MarketFragment();
			}
			if (arg0 == 2) {
				fragment = new PortfolioFragment();
			}
			return fragment;
		}

		@Override
		public int getCount() {
			// TODO Auto-generated method stub
			return 3;
		}
	}



	//Rickard Bremer

	private void checkInternetConnection() {
    	ConnectivityManager connMgr = (ConnectivityManager) 
    	        getSystemService(Context.CONNECTIVITY_SERVICE);
    	    NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();
    	    if (networkInfo != null && networkInfo.isConnected()) {
    	    	System.out.println("A internet connection is available!");
    	    	stockArray = new StockThread().execute();
    	    } else {
    	        System.out.println("A Internet connection is not available!");
    	        System.exit(0);
    	    }
    	}

}
