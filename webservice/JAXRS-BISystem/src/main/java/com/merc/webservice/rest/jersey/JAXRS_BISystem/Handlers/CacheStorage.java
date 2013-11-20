package com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.ektorp.CouchDbConnector;
import org.ektorp.ViewResult.Row;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.Application.BIApplication;

/**
 * Handles caching of data from the {@code db}. Uses an implemented timer to
 * update the cache, where the update periods are controlled by a
 * {@code updateTime}. Implementation support for manual updating of the cache
 * is supported.
 * 
 * Created: 2013-11-09. Modified: 2013-11-15.
 * 
 * @author Robin Larsson
 * @version 0.5
 */
public class CacheStorage {
    private static List<Row> cacheStorage = null;
    private enum StatusCodes {
	NotUpdating(true), CurrentlyUpdating(false);

	private boolean status;

	StatusCodes(boolean status) {
	    this.status = status;
	}
    }
    private StatusCodes currentUpdateStatus = null;

    /**
     * Handles the management of the cache storage. Updates are
     * 
     * @param db
     *            CouchDB database connector
     * @param delay
     *            The time before the first update is done.
     * @param period
     *            The time period before next update shall be performed.
     */
    public CacheStorage(String designDocPath, List<String> viewNames,
	    long delay, long period) {
	/* Creating the timer, used for initiating cache storage updates */
	Timer cacheTimer = new Timer();
	/*
	 * Scheduling when the timer task shall be executed. Executed first when
	 * {@code delay} is reached, then after every milliseconds specified by
	 * {@code period}.
	 */
	cacheTimer.scheduleAtFixedRate(new CacheTask(designDocPath, viewNames),
		delay, period);
    }

    /**
     * Returns the current data in the cache storage. Note: synchronous
     * returning, i.e. not returning until an update is done.
     * 
     * @return A list of {@link org.ektorp.ViewResult.Row}.
     */
    public List<Row> getCurrentCache() {
	/*
	 * Checking whether there is an update in progress or not.
	 */
	if (this.currentUpdateStatus == StatusCodes.CurrentlyUpdating) {
	    while(this.currentUpdateStatus != StatusCodes.NotUpdating) {
		// Do nothing, just postpones the returning
	    }
	    return cacheStorage;
	}
	else { // StatusCodes.NotUpdating
	    return cacheStorage;
	}
    }

    /**
     * Issuing an update of the cache storage.
     * 
     * @param db
     *            A CouchDB database connector object.
     * @param designDocPath
     *            Name of the CouchDB design document.
     * @param viewNames
     *            A list of CouchDB view names.
     */
    public void updateCache(CouchDbConnector db, String designDocPath,
	    List<String> viewNames) {
	/*
	 * Setting the status checker to signal update is in progress
	 */
	this.currentUpdateStatus = StatusCodes.CurrentlyUpdating;
	// Logger.getLogger("JAXRS-BISystem").log(
	// Level.INFO,
	// "Starting updating of cache at ".concat(Long.toString(System
	// .currentTimeMillis())));
	System.out.println("Starting updating of cache at ".concat(Long
		.toString(System.currentTimeMillis())));
	// Clearing the cache storage before the start of the updating
	CacheStorage.cacheStorage.clear();
	// Logger.getLogger("JAXRS-BISystem").log(
	// Level.INFO,
	// "Clearing the cache at ".concat(Long.toString(System
	// .currentTimeMillis())));
	System.out.println("Clearing the cache at ".concat(Long.toString(System
		.currentTimeMillis())));

	for (String i : viewNames) {
	    // Adding data from the db to the cache storage
	    CacheStorage.cacheStorage.addAll(DatabaseHandler
		    .retrieveDataAsRows(db, designDocPath, i));
	    // Logger.getLogger("JAXRS-BISystem").log(
	    // Level.INFO,
	    // "Updated cache for ".concat(i.concat("at").concat(
	    // Long.toString(System.currentTimeMillis()))));
	    System.out.println("Updated cache for ".concat(" at ").concat(
		    Long.toString(System.currentTimeMillis())));
	    System.out.println("Current rows in cache: "
		    + CacheStorage.cacheStorage.size());
	}

	/*
	 * Setting the status checker to signal that no update is in progress
	 */
	this.currentUpdateStatus = StatusCodes.NotUpdating;
    }

    /**
     * Implementation of a {@link java.util.TimerTask}, fit to handling the
     * update of the cache storage.
     * 
     * @author Robin Larsson
     * @version 0.5
     */
    class CacheTask extends TimerTask {

	private CouchDbConnector dbConnection;
	private DatabaseHandler dbHandler;
	private String designDocPath;
	private List<String> viewNames;

	/**
	 * Updates the cache storage, retrieving data from a {@code db} with
	 * supplied {@code designDocPath} and list of {@code viewNames}. The
	 * data is stored in a list of {@link org.ektorp.ViewResult.Row}.
	 * 
	 * @param designDocPath
	 *            Name of the CouchDB design document.
	 * @param viewNames
	 *            A list of CouchDB view names.
	 */
	public CacheTask(String designDocPath, List<String> viewNames) {
	    this.dbHandler = new DatabaseHandler();
	    this.dbConnection = this.dbHandler.getConnector();
	    this.designDocPath = designDocPath;
	    this.viewNames = viewNames;
	}

	/**
	 * Executes the updating of the cache.
	 */
	@Override
	public void run() {
	    /*
	     * Checking whether the {@link CacheStorage.cacheStorage} is not
	     * empty. If not empty, the cache storage is renewed (dropped and
	     * initialised again).
	     */
	    if (CacheStorage.cacheStorage != null) {
		updateCache(this.dbConnection, this.designDocPath,
			this.viewNames);
	    }
	    else { // Only reached when executed the first time
		CacheStorage.cacheStorage = new ArrayList<Row>();
		updateCache(this.dbConnection, this.designDocPath,
			this.viewNames);
	    }
	}

	@Override
	protected void finalize() throws Throwable {
	    /*
	     * Closing the HTTP connection, if not already closed.
	     */
	    super.finalize();
	    this.dbHandler.shutdownConnection();
	}
    }
}