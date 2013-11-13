package com.merc.webservice.rest.jersey.JAXRS_BISystem.Handler;

import java.net.MalformedURLException;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;

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
 * Created: 2013-11-07. Modified: 2013-11-12.
 * 
 * @author Robin Larsson
 * @version 0.5
 * @see https://github.com/helun/Ektorp
 */
public class DatabaseHandler {

    private final String DB_IP = "http://mercury.dyndns.org:5984/";
    private final String DB_NAME = "mercury";
    private HttpClient httpClient;
    private CouchDbInstance dbInstance;
    private CouchDbConnector db;

    /**
     * Initiating a connection to a CouchDB database.
     */
    public DatabaseHandler() {
	/* Standard HTTP connection to the DB interface */
	try {
	    httpClient = new StdHttpClient.Builder().url(DB_IP).build();
	}
	catch (MalformedURLException e) {
	    e.printStackTrace();
	}

	/*
	 * Required CouchDB server communication instance.
	 */
	dbInstance = new StdCouchDbInstance(httpClient);
	/*
	 * Required CouchDB database communication instance. Using the
	 * CouchDBInstance object to communicate with the CouchDB server.
	 */
	db = new StdCouchDbConnector(DB_NAME, dbInstance);
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
     * Returns entity mapped objects in a list, where the objects are based on
     * {@code modelClass}. Queried from {@code db} with the specified
     * {@code viewName} (located in a {@code designDocPath}).
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewName
     *            Name of the CouchDB view.
     * @param modelClass
     *            The model that shall be used for the entity mapping
     * @return A list of entity mapped objects
     */
    public static <T> String retrieveJSONData(CouchDbConnector db,
	    String designDocPath, String viewName, Class<T> modelClass) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 */
	ViewQuery query = new ViewQuery()
		.designDocId(designDocPath)
		.viewName(viewName);
//	List<T> result = db.queryView(query, modelClass);
	List<Row> result = db.queryView(query).getRows();

	return result.toString();
//	return result.toString();
    }

    /**
     * Returns entity mapped objects in a list, where the objects are based on
     * {@code modelClass}. Queried from {@code db} with the specified
     * {@code viewName} (located in a {@code designDocPath}). Query options are
     * used, to filter out which data that shall be returned (done at the
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
     * @param endKey
     *            End key for the view query.
     * @param modelClass
     *            The model that shall be used for the entity mapping
     * @return A list of entity mapped objects
     */
    public static <T> String retrieveJSONData(CouchDbConnector db,
	    String designDocPath, String viewName, String startKey,
	    String endKey, Class<T> modelClass) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 */
	ViewQuery query = new ViewQuery()
		.designDocId(designDocPath)
		.viewName(viewName)
		.startKey(startKey)
		.endKey(endKey);
//	List<T> result = db.queryView(query, modelClass);
	List<Row> result = db.queryView(query).getRows();

	return result.toString();
//	return result.toString();
    }

//    ***Deprecated method***
//    /**
//     * Returns raw JSON data from the specified {@code viewName} (located in a
//     * {@code designDocPath}). Reached via the provided {@code db}.
//     * 
//     * @param db
//     *            A CouchDB database connector object.
//     * @param designDocPath
//     *            Name of the CouchDB design document.
//     * @param viewName
//     *            Name of the CouchDB view.
//     * @return String Raw JSON data.
//     */
//    public static String retrieveRawJSONData(CouchDbConnector db,
//	    String designDocPath, String viewName) {
//	/*
//	 * Executing a query against the database to retrieve data from a view,
//	 * located in the specified design document path.
//	 */
//	ViewQuery query = new ViewQuery().designDocId(designDocPath).viewName(
//		viewName);
//	/* Retrieving the result of the query as a InputStream object */
//	InputStream resultStream = db.queryForStream(query);
//	/* Preparing the InputStream to be read */
//	BufferedReader resultReader = new BufferedReader(new InputStreamReader(
//		resultStream));
//	StringBuilder sb = new StringBuilder();
//	String line = null; // Used for temporary storage
//	/*
//	 * Reading each line (ends with \n) and concatenating it into a result
//	 * String object.
//	 */
//	try {
//	    while ((line = resultReader.readLine()) != null) {
//		sb.append(line + "\n");
//	    }
//	}
//	catch (IOException e1) {
//	    e1.printStackTrace();
//	}
//
//	/* Releasing the resources for the stream */
//	try {
//	    resultStream.close();
//	}
//	catch (IOException e) {
//	    e.printStackTrace();
//	}
//
//	/* Return the result as a String object */
//	return sb.toString();
//    }
}
