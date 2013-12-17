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

		
		
		l.setOnItemClickListener(new OnItemClickListener() {
			public void onItemClick(AdapterView parent, View view, int position, long id) {
				System.out.println(position);
				Log.d("test", testArrayList.get(position));
				
				MainActivity.stockPos = position;
				// System.out.println(STOCKLIST.get(position));
				MainActivity.StockObject = STOCKLIST.get(position);

				Intent StockActivity = new Intent(SearchActivity.this, StockActivity.class);
				MainActivity.StockSymbol = STOCKSYMBOL[position];
				MainActivity.StockName = STOCKNAME[position];
				MainActivity.StockMarket = STOCKMARKET[position];

				startActivity(StockActivity);
				
/*				if (MainActivity.globalArrayTest.contains(testArrayList.get(position))) {
					Context context = getApplicationContext();
					CharSequence text = "Portfolio already contains "+ testArrayList.get(position)+"!";
					int duration = Toast.LENGTH_SHORT;

					Toast toast = Toast.makeText(context, text, duration);
					toast.show();
				} else {
					Context context = getApplicationContext();
					CharSequence text = testArrayList.get(position) + " added to portfolio!";
					int duration = Toast.LENGTH_SHORT;
					MainActivity.globalArrayTest.add(testArrayList.get(position));
					Toast toast = Toast.makeText(context, text, duration);
					toast.show();
				}*/
				

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
			// Log.d("Event", query);

			search(query);
		}
	}

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
