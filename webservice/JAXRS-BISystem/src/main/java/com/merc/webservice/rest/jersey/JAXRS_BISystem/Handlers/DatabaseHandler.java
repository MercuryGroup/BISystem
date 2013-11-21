package com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers;

import java.net.MalformedURLException;
import java.util.List;
import java.util.ArrayList;

import org.ektorp.*;
import org.ektorp.ViewResult.Row;
import org.ektorp.http.StdHttpClient;
import org.ektorp.http.HttpClient;
import org.ektorp.impl.StdCouchDbConnector;
import org.ektorp.impl.StdCouchDbInstance;

/**
 * Handles connections with a CouchDB database. Supports retrieval of data from
 * views, with specific options. Using Ektorp to achieve this.
 * 
 * Created: 2013-11-07. Modified: 2013-11-20.
 * 
 * @author Robin Larsson
 * @version 0.5
 * @see https://github.com/helun/Ektorp
 */
public class DatabaseHandler {

    private final String DB_IP = "http://mercury.dyndns.org:5984/";
    private final String DB_NAME = "mercury";
    private final int CONNECTION_TIMEOUT_SEC = 30;
    private final int SOCKET_TIMEOUT_SEC = 30;
    private final int MAX_CONNECTIONS = 20;
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
		    .maxConnections(MAX_CONNECTIONS).build();
	}
	catch (MalformedURLException e) {
	    e.printStackTrace();
	}
	
	/*
	 * Required CouchDB server communication instance.
	 */
	this.dbInstance = new StdCouchDbInstance(httpClient);
	/*
	 * Required CouchDB database communication instance. Using the
	 * CouchDBInstance object to communicate with the CouchDB server.
	 */
	this.db = new StdCouchDbConnector(DB_NAME, dbInstance);
    }

    /**
     * Performs a shutdown of the HTTP connection.
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
     * Returns a list of {@link org.ektorp.ViewResult.Row} as a result of the
     * query. Queried from {@code db} with the specified {@code viewName}
     * (located in a {@code designDocPath}).
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewName
     *            Name of the CouchDB view.
     * @return A list of {@link org.ektorp.ViewResult.Row}.
     */
    public static List<Row> retrieveDataAsRows(CouchDbConnector db,
	    String designDocPath, String viewName) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	ViewQuery query = new ViewQuery().designDocId(designDocPath).viewName(
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
     * @return A list of {@link org.ektorp.ViewResult.Row}.
     */
    public static List<Row> retrieveDataAsRows(CouchDbConnector db,
	    String designDocPath, String viewName, String startKey,
	    String endKey) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	ViewQuery query = new ViewQuery().designDocId(designDocPath).viewName(
		viewName).rawStartKey(startKey).rawEndKey(endKey).cacheOk(true);
	/*
	 * Returns the result as a list of {@link org.ektorp.ViewResult.Row}.
	 */
	return db.queryView(query).getRows();
    }

    /**
     * Returns the result of the query to the {@code db} as a String,
     * Queried from {@code db} with the specified
     * {@code viewName} (located in a {@code designDocPath}).
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewName
     *            Name of the CouchDB view.
     * @return A String object with the results.
     */
    public static String retrieveJSONData(CouchDbConnector db,
	    String designDocPath, String viewName) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	ViewQuery query = new ViewQuery().designDocId(designDocPath)
		.viewName(viewName).cacheOk(true);
	List<Row> result = db.queryView(query).getRows();

	return result.toString();
    }
    
    /**
     * Returns the result of the query to the {@code db} as a String.
     * Queried from {@code db} with the specified
     * {@code viewName} (located in a {@code designDocPath}). Query options are
     * used to filter out which data that shall be returned (done at the
     * database): {@code startKey} & {@code endKey}.
     * 
     * @param db
     *            A CouchDB database connector object.
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
     * @return A String object with the results.
     */
    private static String retrieveJSONData(CouchDbConnector db,
	    String designDocPath, String viewName, String startKey,
	    String endKey) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 * 
	 * Caching is enabled by the Ektorp library.
	 */
	ViewQuery query = new ViewQuery().designDocId(designDocPath)
		.viewName(viewName).rawStartKey(startKey).rawEndKey(endKey)
		.cacheOk(true);
	List<Row> result = db.queryView(query).getRows();

	return result.toString();
    }

    /**
     * Returns the result of the query to the {@code db} as a String.
     * Queried from {@code db} with the specified
     * {@code viewNames} (located in a {@code designDocPath}). It's possible to
     * concatenate data from multiple views, by changing {@code multipleViews}
     * Query options are used, to filter out which data that shall be returned
     * (done at the database): {@code startKey} & {@code endKey}.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewNames
     *            Names of the CouchDB views.
     * @param startKey
     *            Start key for the view query.
     *            Converted to a JSON object/array.
     * @param endKey
     *            End key for the view query.
     *            Converted to a JSON object/array.
     * @param multipleViews
     *            Specifies whether data shall be concatenated from multiple
     *            views.
     * @return A String object with the results.
     */
    public static String retrieveJSONData(CouchDbConnector db,
	    String designDocPath, String viewNames, String startKey,
	    String endKey, boolean multipleViews) {

	if (multipleViews) { // Multiple views to retrieve
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
			retrieveDataAsRows(db, designDocPath,
				view.trim(), startKey, endKey));
	    }
	    
	    return tempList.toString();
	}
	else { // Single view to retrieve
	    return retrieveJSONData(db, designDocPath, viewNames,
		    startKey, endKey);
	}
    }
}
