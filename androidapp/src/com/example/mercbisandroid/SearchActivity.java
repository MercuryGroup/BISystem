package com.example.mercbisandroid;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import android.app.Activity;
import android.app.SearchManager;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.widget.ArrayAdapter;
import android.widget.ListView;

public class SearchActivity extends Activity {

	ListView l;

	String[] testArray;
	List<String> testArrayList;
	ArrayAdapter<String> adapter;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_search);

		testArray = getResources().getStringArray(R.array.list);
		testArrayList = new ArrayList<String>(Arrays.asList(testArray));

		l = (ListView) findViewById(R.id.list);
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

}
