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
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.ListView;

public class SearchActivity extends ListActivity {

	ListView l;

	String[] Stocks;
	List<String> testArrayList;
	ArrayAdapter<String> adapter;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_search);

		getListOfStocks();

		// Stocks = getResources().getStringArray(R.layout.list_stocks);

		testArrayList = new ArrayList<String>(Arrays.asList(Stocks));

		l = (ListView) findViewById(android.R.id.list);
		adapter = new ArrayAdapter<String>(this,
				android.R.layout.simple_list_item_1, testArrayList);
		l.setAdapter(adapter);

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

	public void onListItemClick(ListView l, View v, int position, long id) {
		Log.d("Test", testArrayList.get(position));
		MainActivity test = new MainActivity();
		test.addToArray(testArrayList.get(position));
	}

	public void getListOfStocks() {

		AsyncTask<ArrayList<Object>, Void, ArrayList<Object>> execute = new StockThread().execute();

		try {
			Stocks = new String[execute.get().size()];
		} catch (InterruptedException e1) {
			e1.printStackTrace();
		} catch (ExecutionException e1) {
			e1.printStackTrace();
		}

		try {
			for (int i = 0; i < execute.get().size(); i++) {
				String JsonLine = execute.get().get(i).toString();
				JSONObject JOBJ;
				try {

					JOBJ = new JSONObject(JsonLine);
					Stocks[i] = JOBJ.getString("symbol");
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
		return;

	}

}
