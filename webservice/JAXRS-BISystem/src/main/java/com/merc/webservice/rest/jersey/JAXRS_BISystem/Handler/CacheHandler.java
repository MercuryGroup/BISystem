package com.merc.webservice.rest.jersey.JAXRS_BISystem.Handler;

import java.util.Date;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import org.ektorp.CouchDbConnector;

/**
 * Handles caching of data from the {@code db}. Uses an implemented timer to
 * update the cache, where the update periods are controlled by a
 * {@code updateTime}.
 * 
 * Created: 2013-11-09. Modified: 2013-11-12.
 * 
 * @author Robin Larsson
 * @version 0.5
 */
public class CacheHandler {
    /**
     * Handles the management of the cache storage.
     * Updates are 
     * @param db
     *            CouchDB database connector
     * @param updateTime
     *            The time when the cache shall be updated
     */
    public CacheHandler(CouchDbConnector db, Date updateTime) {
	/* Creating the timer, used for initiating cache storage updates */
	Timer cacheTimer = new Timer();
	/* 
	 * Scheduling when the timer task shall be executed.
	 * Executed every day when {@code updateTime} is reached.
	 */
	cacheTimer.schedule(new CacheTask(db), updateTime);
    }

    /**
     * Requests an update of the cache storage. As well used for initiating a
     * first cache update.
     */
    public void updateCache() {

    }

    /**
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
	 * supplied {@code designDocPath} and list of {@code viewNames}.
	 * 
	 * @param db
	 *            CouchDB database connector
	 */
	public CacheTask(CouchDbConnector db) {
	    
	}

	/**
	 * Executes
	 */
	@Override
	public void run() {

	}
    }

}
