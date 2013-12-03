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
//import com.example.mercbisandroid.*;

public class StockActivity extends FragmentActivity implements TabListener {
	
	private ArrayList<String> globalArrayTest = new ArrayList<String>();
	
	ActionBar actionBar;
	ViewPager viewPager;

	@Override
	protected void onCreate(Bundle arg0) {
		super.onCreate(arg0);
		setContentView(R.layout.activity_main);

		
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

		ActionBar.Tab info = actionBar.newTab();
		info.setText("Info");
		info.setTabListener(this);

		ActionBar.Tab graph = actionBar.newTab();
		graph.setText("Graphs");
		graph.setTabListener(this);

		ActionBar.Tab news = actionBar.newTab();
		news.setText("News");
		news.setTabListener(this);

		actionBar.addTab(info);
		actionBar.addTab(graph);
		actionBar.addTab(news);
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
				fragment = new StockInfoFragment();
			}
			if (arg0 == 1) {
				fragment = new StockGraphFragment();
			}
			if (arg0 == 2) {
				fragment = new StockNewsFragment();
			}
			return fragment;
		}

		@Override
		public int getCount() {
			// TODO Auto-generated method stub
			return 3;
		}
	}
	}



	//Rickard Bremer

//package com.example.mercbisandroid;
//
//import java.util.ArrayList;
//import java.util.concurrent.ExecutionException;
//
//import org.afree.chart.ChartFactory;
//import org.afree.chart.plot.PlotOrientation;
//import org.afree.data.category.CategoryDataset;
//import org.afree.data.category.DefaultCategoryDataset;
//import org.json.JSONException;
//import org.json.JSONObject;
//
//import android.app.Activity;
//import android.os.AsyncTask;
//import android.os.Bundle;
//import android.view.ViewGroup;
//
//public class StockActivity extends Activity {
//	
//	public AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> DetailedStockArray;
//	
//	@Override
//    public void onCreate(Bundle savedInstanceState) {   
//       super.onCreate(savedInstanceState);    
//       setContentView(R.layout.activity_stock);
//       DetailedStockArray = new DetailedStockThread().execute();
//     
//       CategoryDataset dataset = CreateDataSet(DetailedStockArray);
//       ViewGroup viewGroup = (ViewGroup)getWindow().getDecorView().findViewById(android.R.id.content);
//       ChartView chartView = new ChartView(this);
//       chartView.drawChart(ChartFactory.createLineChart(MainActivity.StockSymbol,"Date","Value",dataset,PlotOrientation.VERTICAL,true,true,false));       
//
//       viewGroup.addView(chartView);
//       
//   }
//
//public static CategoryDataset CreateDataSet(AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> detailedStockArray){
//	
//	DefaultCategoryDataset dataset = new DefaultCategoryDataset();
//	
//	try {
//		System.out.println(detailedStockArray.get().size());
//	} catch (InterruptedException e1) {
//		// TODO Auto-generated catch block
//		e1.printStackTrace();
//	} catch (ExecutionException e1) {
//		// TODO Auto-generated catch block
//		e1.printStackTrace();
//	}
//	
//	try {
//		for(int i = 0; i < detailedStockArray.get().size(); i++){
//		 
//		JSONObject JOBJ = new JSONObject(detailedStockArray.get().get(i).toString());
//		
//		
//		String latest = JOBJ.getString("latest");
//		String updated = JOBJ.getString("updated");
//		System.out.println("latest :" + latest + " # "+" updated : " + updated);
//		
//		//dataset.addValue(Float.parseFloat(latest), "Value", updated);
//		dataset.addValue(Float.parseFloat(latest), "Value", "updated");
//		}
//	} catch (NumberFormatException e) {
//		// TODO Auto-generated catch block
//		e.printStackTrace();
//	} catch (InterruptedException e) {
//		// TODO Auto-generated catch block
//		e.printStackTrace();
//	} catch (ExecutionException e) {
//		// TODO Auto-generated catch block
//		e.printStackTrace();
//	} catch (JSONException e) {
//		// TODO Auto-generated catch block
//		e.printStackTrace();
//	}
//	return dataset; 
//}
//
//
//}
