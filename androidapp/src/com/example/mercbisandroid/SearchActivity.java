package com.example.mercbisandroid;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ExecutionException;

import org.json.JSONException;
import org.json.JSONObject;

import android.app.ListActivity;
import android.app.SearchManager;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Toast;

/**
 * Search activity which takes care of all the search queries. 
 * Search results can be clicked on to show a detailed view of the stock.
 * @author Justin In�cio
 */
public class SearchActivity extends ListActivity {
	ListView l;

	String[] Stocks;
	JSONObject JOBJ = new JSONObject();
	List<String> testArrayList;
	ArrayAdapter adapter;
	public ArrayList<JSONObject> STOCKLIST = new ArrayList<JSONObject>();
	String[] STOCKNAME, STOCKSYMBOL, STOCKMARKET;

	@SuppressWarnings("unchecked")
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_search);

		try {
			STOCKNAME = new String[MainActivity.stockArray.get().size()];
			STOCKSYMBOL = new String[MainActivity.stockArray.get().size()];
			STOCKMARKET = new String[MainActivity.stockArray.get().size()];

		} catch (InterruptedException e1) {
			e1.printStackTrace();
		} catch (ExecutionException e1) {
			e1.printStackTrace();
		}

		try {
			Stocks = new String[MainActivity.stockArray.get().size()];
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			for (int i = 0; i < MainActivity.stockArray.get().size(); i++) {
				String JsonLine = MainActivity.stockArray.get().get(i).toString();
				try {

					JOBJ = new JSONObject(JsonLine);
					STOCKLIST.add(JOBJ);
					Stocks[i] = STOCKLIST.get(i).getString("name");
					STOCKNAME[i] = STOCKLIST.get(i).getString("name");
					STOCKSYMBOL[i] = STOCKLIST.get(i).getString("symbol");
					STOCKMARKET[i] = STOCKLIST.get(i).getString("market");

				} catch (JSONException e) {

					e.printStackTrace();
				}
			}
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		testArrayList = new ArrayList<String>(Arrays.asList(Stocks));
		l = (ListView) findViewById(android.R.id.list);
		adapter = new ArrayAdapter(this, android.R.layout.simple_list_item_1, testArrayList);
		l.setAdapter(adapter);

		
		/**
		 * onclick listener which launches the detailed stock view of the clicked stock name.
		 */
		l.setOnItemClickListener(new OnItemClickListener() {
			public void onItemClick(AdapterView parent, View view, int position, long id) {
/*				System.out.println(position);
				Log.d("test", testArrayList.get(position));*/
				
				MainActivity.stockPos = position;
				MainActivity.StockObject = STOCKLIST.get(position);

				Intent StockActivity = new Intent(SearchActivity.this, StockActivity.class);
				MainActivity.StockSymbol = STOCKSYMBOL[position];
				MainActivity.StockName = STOCKNAME[position];
				MainActivity.StockMarket = STOCKMARKET[position];

				startActivity(StockActivity);
			}
		});

		handleIntent(getIntent());
	}

	protected void onNewIntent(Intent intent) {
		handleIntent(intent);
	}

	private void handleIntent(Intent intent) {
		if (Intent.ACTION_SEARCH.equals(intent.getAction())) {
			String query = intent.getStringExtra(SearchManager.QUERY);
			search(query);
		}
	}

	
	/**
	 * method which tells how to display the returned listview after a query.
	 * In this case, everything which does not contain the elements of 'query', is removed.
	 * @param query the variable which holds the user's search query.
	 */
	private void search(String query) {
		Iterator<String> iter = testArrayList.iterator();
		while (iter.hasNext()) {
			if (!iter.next().contains(query)) {
				iter.remove();
			}
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.search, menu);

		return true;
	}


	
}
