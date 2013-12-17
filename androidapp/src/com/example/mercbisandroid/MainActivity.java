package com.example.mercbisandroid;
import java.util.ArrayList;
import java.util.concurrent.ExecutionException;

import org.afree.chart.ChartFactory;
import org.afree.chart.plot.PlotOrientation;
import org.json.JSONException;
import org.json.JSONObject;

import android.app.ActionBar;
import android.app.ActionBar.Tab;
import android.app.ActionBar.TabListener;
import android.app.FragmentTransaction;
import android.app.SearchManager;
import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.ParseException;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.view.ViewPager;
import android.view.Menu;
import android.view.View;
import android.widget.RadioButton;
import android.widget.SearchView;
import android.widget.TextView;


public class MainActivity extends FragmentActivity implements TabListener {
	
	public static AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> MarketArray;
	public static AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> stockArray;
	String MarketTitle;
	String chart;
	
	long thirtyDays;
	
	public static String StockSymbol , StockName, StockMarket, Market;
	public static int stockPos;
	public static JSONObject StockObject;
	public static long StockTime;
	public static long MarketTime;
	
	public static ArrayList<String> globalArrayTest = new ArrayList<String>();
	
	ActionBar actionBar;
	ViewPager viewPager;

	@Override
	
	protected void onCreate(Bundle arg0) {
		
		super.onCreate(arg0);
		
		setContentView(R.layout.activity_main);
		

		//Rickard Bremer
		MarketTitle = "LSE";
		Market = "lse_market";
		chart = "linechart";
		thirtyDays = System.currentTimeMillis()/10;
		MarketTime = thirtyDays - 3 * 24 * 60 * 60 * 1000;
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
	
	public void onRadioButtonChart(View view) throws JSONException, InterruptedException, ExecutionException, ParseException, java.text.ParseException {
	    // Is the button now checked?
	    boolean checked = ((RadioButton) view).isChecked();
	    MarketArray = new MarketThread().execute();
	    
	    // Check which radio button was clicked
	    switch(view.getId()) {
	        case R.id.barChartMarket:
	            if (checked)
	            	chart = "barchart";
	            	MarketFragment.chartView.drawChart(ChartFactory.createBarChart(MarketTitle,"Time", "Value", MarketFragment.createDatasetBarChart(), PlotOrientation.VERTICAL, true, true, false));
	            break;
	        case R.id.candleStickMarket:
	            if (checked)
	            	chart = "candlestick";
		            MarketFragment.chartView.drawChart(ChartFactory.createCandlestickChart(MarketTitle,"Time", "Value", MarketFragment.createCandleStickDataset(), false));
	            break;
	            
	        case R.id.lineChartMarket:
	            if (checked)
	            	chart = "linechart";
	            	MarketFragment.chartView.drawChart(ChartFactory.createLineChart(MarketTitle,"Time", "Value", MarketFragment.createDatasetLineChart(), PlotOrientation.VERTICAL, true, true, false));	            	
	            break;
	    }
	}
	public void onRadioButtonTime(View view) throws JSONException, InterruptedException, ExecutionException, ParseException, java.text.ParseException {
	    // Is the button now checked?
	    boolean checked = ((RadioButton) view).isChecked();
	    
	    // Check which radio button was clicked
	    switch(view.getId()) {
	        case R.id.oneday:
	            if (checked)
	            	
	            	MarketTime = System.currentTimeMillis() - 1 * 24 * 60 * 60 * 1000;
            		MarketArray = new MarketThread().execute();
      
            		
            		if(chart.equals("linechart")){
    	    			
            			MarketFragment.dataset = MarketFragment.createDatasetLineChart();
    	    			MarketFragment.chartView.drawChart(ChartFactory.createLineChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));	
            		
            		}
            		
            		
            		if(chart.equals("barchart")){
            			
            			MarketFragment.dataset = MarketFragment.createDatasetBarChart();
            			MarketFragment.chartView.drawChart(ChartFactory.createBarChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));    
            		
            		}
            		
            		if(chart.equals("candlestick")){
            			
            			MarketFragment.datacandle = MarketFragment.createCandleStickDataset();
            			MarketFragment.chartView.drawChart(ChartFactory.createCandlestickChart(MarketTitle, "Time", "Value", MarketFragment.datacandle, false));
            		
            		}
	               
	            break;
	        case R.id.oneweek:
	            if (checked)
	            
	            	MarketTime = System.currentTimeMillis() - 7 * 24 * 60 * 60 * 1000;
        		    MarketArray = new MarketThread().execute();
  
        		
        		if(chart.equals("linechart")){
	    			
        			MarketFragment.dataset = MarketFragment.createDatasetLineChart();
	    			MarketFragment.chartView.drawChart(ChartFactory.createLineChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));	
        		
        		}
        		 
        		
        		if(chart.equals("barchart")){
        			
        			MarketFragment.dataset = MarketFragment.createDatasetBarChart();
        			MarketFragment.chartView.drawChart(ChartFactory.createBarChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));    
        		
        		}
        		
        		if(chart.equals("candlestick")){
        			
        			MarketFragment.datacandle = MarketFragment.createCandleStickDataset();
        			MarketFragment.chartView.drawChart(ChartFactory.createCandlestickChart(MarketTitle, "Time", "Value", MarketFragment.datacandle, false));
        		
        		}
	            	
	            break;
	            
	        case R.id.thirtydays:
	            if (checked)
	            	thirtyDays = System.currentTimeMillis()/10;
					MarketTime = thirtyDays - 3 * 24 * 60 * 60 * 1000;
        		
	            	MarketArray = new MarketThread().execute();
  
        		
        		if(chart.equals("linechart")){
	    			
        			MarketFragment.dataset = MarketFragment.createDatasetLineChart();
	    			MarketFragment.chartView.drawChart(ChartFactory.createLineChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));	
        		
        		}
        		
        		
        		if(chart.equals("barchart")){
        			
        			MarketFragment.dataset = MarketFragment.createDatasetBarChart();
        			MarketFragment.chartView.drawChart(ChartFactory.createBarChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));    
        		
        		}
        		
        		if(chart.equals("candlestick")){
        			
        			MarketFragment.datacandle = MarketFragment.createCandleStickDataset();
        			MarketFragment.chartView.drawChart(ChartFactory.createCandlestickChart(MarketTitle, "Time", "Value", MarketFragment.datacandle, false));
        		
        		}
        		
	            break;
	    }
	}
public void onClick(View view) throws JSONException, InterruptedException, ExecutionException {
	    switch(view.getId()){
	        case R.id.button:
	        	
	        	Market = "nyse_market";
	        	MarketTitle = "NYSE";
	        	thirtyDays = System.currentTimeMillis()/10;
				MarketTime = thirtyDays - 3 * 24 * 60 * 60 * 1000;
				MarketArray = new MarketThread().execute();
				MarketFragment.dataset = MarketFragment.createDatasetLineChart();
    			MarketFragment.chartView.drawChart(ChartFactory.createLineChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));
    			JSONObject MarketObj = new JSONObject(MarketArray.get().get(0).toString());
   			    TextView t = new TextView(this); 
   			    t=(TextView)findViewById(R.id.textView1); 
   			    t.setText(" Market : " + MarketObj.getString("market") + " Percent : " + MarketObj.getString("percent") + " OpenVal : " + MarketObj.getString("openVal"));
				
   			    break;
	    }   
	}


	public void onClick1(View view) throws JSONException, InterruptedException, ExecutionException {
	    switch(view.getId()){
	        case R.id.button1:
	        	Market = "lse_market";
	        	MarketTitle = "LSE";
	        	thirtyDays = System.currentTimeMillis()/10;
				MarketTime = thirtyDays - 3 * 24 * 60 * 60 * 1000;
				MarketArray = new MarketThread().execute();
				MarketFragment.dataset = MarketFragment.createDatasetLineChart();
    			MarketFragment.chartView.drawChart(ChartFactory.createLineChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));
    			JSONObject MarketObj = new JSONObject(MarketArray.get().get(0).toString());
    			 TextView t = new TextView(this); 
    			 t=(TextView)findViewById(R.id.textView1); 
    			 t.setText(" Market : " + MarketObj.getString("market") + " Percent : " + MarketObj.getString("percent") + " OpenVal : " + MarketObj.getString("openVal"));
    			
    			 break;
	    }   
	}

	public void onClick2(View view) throws JSONException, InterruptedException, ExecutionException {
	    switch(view.getId()){
	        case R.id.button2:
	        	Market = "omx_market";
	        	MarketTitle = "OMX";
	        	thirtyDays = System.currentTimeMillis()/10;
				MarketTime = thirtyDays - 3 * 24 * 60 * 60 * 1000;
				MarketArray = new MarketThread().execute();
				MarketFragment.dataset = MarketFragment.createDatasetLineChart();
    			MarketFragment.chartView.drawChart(ChartFactory.createLineChart(MarketTitle,"Time", "Value", MarketFragment.dataset, PlotOrientation.VERTICAL, true, true, false));
    			JSONObject MarketObj = new JSONObject(MarketArray.get().get(0).toString());
   			    TextView t = new TextView(this); 
   			    t=(TextView)findViewById(R.id.textView1); 
   			    t.setText(" Market : " + MarketObj.getString("market") + " Percent : " + MarketObj.getString("percent") + " OpenVal : " + MarketObj.getString("openVal"));
    			
   			    break;
	    }   
	}

}
