/**
 * 
 */
package com.merc.webservice.rest.jersey.JAXRS_BISystem;

import java.net.MalformedURLException;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.ektorp.*;
import org.ektorp.http.StdHttpClient;
import org.ektorp.http.HttpClient;
import org.ektorp.impl.StdCouchDbConnector;
import org.ektorp.impl.StdCouchDbInstance;

/**
 * Handles connections with a CouchDB database. Supports retrieval of data from
 * views. Using Ektorp to achieve this.
 * 
 * Created: 2013-11-07. Modified: 2013-11-07.
 * 
 * @author Robin Larsson
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
	} catch (MalformedURLException e) {
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
     * Returns raw JSON data from the specified view (located in a design
     * document). Reached via the provided CouchDB database connector object.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewName
     *            Name of the CouchDB view.
     * @return String Raw JSON data.
     */
    public static String retrieveRawJSONData(CouchDbConnector db,
	    String designDocPath, String viewName) {
	/*
	 * Executing a query against the database to retrieve data from a view,
	 * located in the specified design document path.
	 */
	ViewQuery query = new ViewQuery().designDocId(designDocPath).viewName(
		viewName);
	/* Retrieving the result of the query as a InputStream object */
	InputStream resultStream = db.queryForStream(query);
	BufferedReader resultReader = new BufferedReader(new InputStreamReader(
		resultStream));
	StringBuilder sb = new StringBuilder();
	String line = null; // Used for temporary storage
	/*
	 * Reading each line (ends with \n) and concatenating it into a result
	 * String object.
	 */
	try {
	    while ((line = resultReader.readLine()) != null) {
		sb.append(line + "\n");
	    }
	} catch (IOException e1) {
	    e1.printStackTrace();
	}
	
	/* Releasing the resources for the stream */
	try {
	    resultStream.close();
	} catch (IOException e) {
	    e.printStackTrace();
	}
	/* Return the result as a String object */
	return sb.toString();
    }
}
