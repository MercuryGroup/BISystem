package com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.ektorp.CouchDbConnector;
import org.ektorp.ViewResult.Row;

/**
 * Handles caching of data from the {@code db}. Uses an implemented timer to
 * update the cache, where the update periods are controlled by a
 * {@code updateTime}.
 * 
 * Created: 2013-11-09. Modified: 2013-11-15.
 * 
 * @author Robin Larsson
 * @version 0.5
 */
public class CacheHandler {
    private static List<Row> cacheStorage = null;

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
    public CacheHandler(String designDocPath, List<String> viewNames,
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
	// Clearing the cache before the start of the updating
	CacheHandler.cacheStorage.clear();
	for (String i : viewNames) {
	    // Adding data from the db in the cache
	    CacheHandler.cacheStorage.addAll(DatabaseHandler
		    .retrieveDataAsRows(db, designDocPath, i));
	}
    }

    /**
     * Implementation of a {@link java.util.TimerTask}, fit to handling the
     * update of the cache storage.
     * 
     * @author Robin Larsson
     * @version 0.5
     */
    class CacheTask extends TimerTask {
	private CouchDbConnector db;
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
	    this.db = new DatabaseHandler().getConnector();
	    this.designDocPath = designDocPath;
	    this.viewNames = viewNames;
	}

	/**
	 * Executes the updating of the cache.
	 */
	@Override
	public void run() {
	    /*
	     * Checking whether the {@link CacheHandler.cacheStorage} is not
	     * empty. If not empty, the cache storage is renewed (dropped and
	     * initialised again).
	     */
	    if (CacheHandler.cacheStorage != null) {
		updateCache(this.db, this.designDocPath, this.viewNames);
	    }
	    else { // Only reached when executed the first time
		CacheHandler.cacheStorage = new ArrayList<Row>();
		updateCache(this.db, this.designDocPath, this.viewNames);
	    }
	}
    }
}