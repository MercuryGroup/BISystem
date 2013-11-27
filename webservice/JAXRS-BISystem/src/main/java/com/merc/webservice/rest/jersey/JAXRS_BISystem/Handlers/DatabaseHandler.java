package com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers;

import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;

import org.ektorp.*;
import org.ektorp.ViewResult.Row;
import org.ektorp.http.StdHttpClient;
import org.ektorp.http.HttpClient;
import org.ektorp.impl.StdCouchDbConnector;
import org.ektorp.impl.StdCouchDbInstance;

/**
 * Handles connections with the CouchDB database. Supports retrieval of data from
 * views, with specific options. Using Ektorp to achieve this.
 * 
 * Created: 2013-11-07. Modified: 2013-11-27.
 * 
 * @author Robin Larsson
 * @version 0.9
 * @see https://github.com/helun/Ektorp
 */
public class DatabaseHandler {
    // For testing purposes
    private final String DB_IP = "http://mercury.dyndns.org:5984/";
    
//    private final String DB_IP = "http://localhost:5984/";
    private final String DB_NAME = "mercury";
    private final int CONNECTION_TIMEOUT_SEC = 30;
    private final int SOCKET_TIMEOUT_SEC = 30;
    private final int MAX_CONNECTIONS = 20;
    private final int MAX_CACHE_ENTRIES = 100000;
    private final long MAX_OBJECT_SIZE = 104857600L; // 100 MB
    private HttpClient httpClient;
    private CouchDbInstance dbInstance;
    private CouchDbConnector db;

    /**
     * Initiating a connection to a CouchDB database.
     */
    public DatabaseHandler() {
	/* Standard HTTP connection to the DB interface */
	try {
	    this.httpClient = new StdHttpClient.Builder().url(DB_IP)
		    .connectionTimeout(CONNECTION_TIMEOUT_SEC * 1000)
		    .socketTimeout(SOCKET_TIMEOUT_SEC * 1000)
		    .maxConnections(MAX_CONNECTIONS)
		    /* 
		     * Caching at 100MB.
		     * Modified with 100000 cache entries to be sure.
		     */
		    .caching(true) // Specifying it for clarification
		    .maxCacheEntries(MAX_CACHE_ENTRIES)
		    .maxObjectSize(MAX_OBJECT_SIZE)
		    .build();
	}
	catch (MalformedURLException e) {
	    e.printStackTrace();
	}
	
	/*
	 * Required CouchDB server communication instance.
	 */
	this.dbInstance = new StdCouchDbInstance(httpClient);
	/*
	 * Required CouchDB database communication instance. The
	 * CouchDBInstance object is used for pre-DB checks with
	 * the CouchDB server.
	 */
	this.db = new StdCouchDbConnector(DB_NAME, dbInstance);
    }

    /**
     * Performs a shutdown of the HTTP client.
     */
    public void shutdownConnection() {
	this.httpClient.shutdown();
    }

    /**
     * Gets the created CouchDB database connector.
     * 
     * @return {@link StdCouchDbConnector} A CouchDB database connector
     */
    public CouchDbConnector getConnector() {
	return db;
    }
    

    /**
     * Returns the result of the query to the {@code db} as a String,
     * Queried from {@code db} with the specified
     * {@code viewName} (located in a {@code designDocPath}) in the
     * {@link org.ektorp.ViewQuery}.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param query
     * 		  An instantiated {@link org.ektorp.ViewQuery} object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewName
     *            Name of the CouchDB view.
     * @return A String object with the results.
     */
    public String retrieveJSONData(CouchDbConnector db, ViewQuery query,
	    String designDocPath, String viewName) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	return retrieveDataAsRows(db, query, designDocPath, viewName).toString();
    }
    
    /**
     * Returns the result of the query to the {@code db} as a String,
     * Queried from {@code db} with the specified
     * {@code viewName} (located in a {@code designDocPath}) in the
     * {@code query}. It's possible to concatenate data from multiple
     * views, by changing {@code multipleViews}.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param query
     * 		  An instantiated {@link org.ektorp.ViewQuery} object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewNames
     *            Name of the CouchDB view.
     * @param multipleViews
     *            Specifies whether data shall be concatenated from multiple
     *            views.
     * @return A String object with the results.
     */
    public String retrieveJSONData(CouchDbConnector db, ViewQuery query,
	    String designDocPath, String viewNames, boolean multipleViews) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	if (multipleViews) {
	    List<Row> tempList = new ArrayList<Row>();
	    /*
	     * Executing a queries against the database to retrieve data from a
	     * all the specified views, located in the specified design document
	     * path.
	     */
	    for (String view : viewNames.split(",")) {
		// Adding the currently made query result to the temporary
		// ArrayList object
//		System.out.println(tempQuery.getDbPath());
		tempList.addAll(
			retrieveDataAsRows(db, query, designDocPath,
				view.trim()));
	    }
	    
	    return tempList.toString();
	}
	else {
	    return retrieveDataAsRows(db, query, designDocPath, viewNames).toString();
	}
    }
    
    /**
     * Returns the result of the query to the {@code db} as a String.
     * Queried from {@code db} with the specified
     * {@code viewNames} (located in a {@code designDocPath})
     * in the {@code query}}. It's possible to concatenate data from multiple
     * views, by changing {@code multipleViews}.
     * Query options are used, to filter out which data that shall be returned
     * (done at the database): {@code startKey} & {@code endKey}.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param query
     * 		  A {@link org.ektorp.ViewQuery} object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewNames
     *            Name of the CouchDB view(s).
     * @param startKey
     *            Start key for the view query.
     * @param endKey
     *            End key for the view query.
     * @param JSONKey
     * 		  Specifies whether the key shall be converted
     * 		  to a JSON object/array.
     * @param multipleViews
     *            Specifies whether data shall be concatenated from multiple
     *            views.
     * @return A String object with the results.
     */
    public String retrieveJSONData(CouchDbConnector db,
	    ViewQuery query,
	    String designDocPath, String viewNames, String startKey,
	    String endKey, boolean JSONKey, boolean multipleViews) {
	List<Row> result = null;
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	// Keys shall be converted to JSON format and multiple views is to be returned
	if (JSONKey && multipleViews) {
	    List<Row> tempList = new ArrayList<Row>();
	    /*
	     * Executing a queries against the database to retrieve data from a
	     * all the specified views, located in the specified design document
	     * path. The keys are parsed as JSON objects.
	     * 
	     * Caching is enabled by the Ektorp library.
	     */
	    for (String view : viewNames.split(",")) {
		// Adding the currently made query result to the temporary
		// ArrayList object
//		System.out.println(tempQuery.getDbPath());
		tempList.addAll(
			retrieveDataAsRows(db, query, designDocPath,
				view.trim(), startKey, endKey, true));
	    }
	    
	    return tempList.toString();
	}
	// Keys shall only be converted to JSON format
	else if (JSONKey && !multipleViews) {
	    result = retrieveDataAsRows(db, query, designDocPath, viewNames,
		    startKey, endKey, true);
	}
	// Multiple views are only to be returned
	else if (!JSONKey && multipleViews) {
	    List<Row> tempList = new ArrayList<Row>();
	    /*
	     * Executing a queries against the database to retrieve data from a
	     * all the specified views, located in the specified design document
	     * path. The keys are parsed as JSON objects.
	     * 
	     * Caching is enabled by the Ektorp library.
	     */
	    for (String view : viewNames.split(",")) {
		// Adding the currently made query result to the temporary
		// ArrayList object
//		System.out.println(tempQuery.getDbPath());
		tempList.addAll(
			retrieveDataAsRows(db, query, designDocPath,
				view.trim(), startKey, endKey, false));
	    }
	    
	    return tempList.toString();
	}
	else { // Not converted and no multiple views are going to be returned
	    query.designDocId(designDocPath)
		    .viewName(viewNames).startKey(startKey).endKey(endKey)
		    .cacheOk(true);
	    result = db.queryView(query).getRows();
	}

	return result.toString();
    }
    
    /**
     * Returns a list of {@link org.ektorp.ViewResult.Row} as a result of the
     * query. Queried from {@code db} with the specified {@code viewName}
     * (located in a {@code designDocPath}) in the {@code query}.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param query
     * 		  An instantiated {@link org.ektorp.ViewQuery} object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewName
     *            Name of the CouchDB view.
     * @return A list of {@link org.ektorp.ViewResult.Row}.
     */
    private List<Row> retrieveDataAsRows(CouchDbConnector db, ViewQuery query,
	    String designDocPath, String viewName) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	query.designDocId(designDocPath).viewName(
		viewName).cacheOk(true);
	/*
	 * Returns the result as a list of {@link org.ektorp.ViewResult.Row}.
	 */
	return db.queryView(query).getRows();
    }
    
    /**
     * Returns a list of {@link org.ektorp.ViewResult.Row} as a result of the
     * query. Queried from {@code db} with the specified
     * {@code viewName} (located in a {@code designDocPath}). Query options are
     * used to filter out which data that shall be returned (done at the
     * database): {@code startKey} & {@code endKey}.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param query
     * 		  An instantiated {@link org.ektorp.ViewQuery} object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewName
     *            Name of the CouchDB view.
     * @param startKey
     *            Start key for the view query.
     *            Converted to a JSON object/array.
     * @param endKey
     *            End key for the view query.
     *            Converted to a JSON object/array.
     * @param JSONKey
     * 		  Specifies whether the key shall be converted
     * 		  to a JSON object/array.
     * @return A list of {@link org.ektorp.ViewResult.Row}.
     */
    private List<Row> retrieveDataAsRows(CouchDbConnector db,
	    ViewQuery query,
	    String designDocPath, String viewName, String startKey,
	    String endKey, boolean JSONKey) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	if (JSONKey) {
	    query.designDocId(designDocPath).viewName(
		    viewName).rawStartKey(startKey).rawEndKey(endKey).cacheOk(true);
	}
	else {
	    query.designDocId(designDocPath).viewName(
		    viewName).startKey(startKey).endKey(endKey).cacheOk(true);
	}
	/*
	 * Returns the result as a list of {@link org.ektorp.ViewResult.Row}.
	 */
	return db.queryView(query).getRows();
    }
}
