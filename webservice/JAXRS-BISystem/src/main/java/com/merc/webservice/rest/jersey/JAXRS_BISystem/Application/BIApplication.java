package com.merc.webservice.rest.jersey.JAXRS_BISystem.Application;

//import java.util.Arrays;
//import java.util.List;
//import java.util.ArrayList;

import org.glassfish.jersey.server.ResourceConfig;

//import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers.CacheStorage;

/**
 * {@link javax.ws.rs.core.Application} class for JAXRS-BISystem.
 * Implements Jerseys' {@link org.glassfish.jersey.server.ResourceConfig}.
 * 
 * Created: 2013-11-10. Modified: 2013-11-20.
 * @author Robin Larsson
 * @version 0.9
 */
public class BIApplication extends ResourceConfig {
//    private final String DB_DESIGNDOC = "_design/bi";
//    private final List<String> DB_VIEWNAMES = new ArrayList<String>(
//	    Arrays.asList("nyse", "nyse_market", "omx", "omx_market", "lse",
//		    "lse_market"));
//    private final long UPDATE_FIRST_DELAY_SEC = 5;
//    private final int UPDATE_PERIOD_HR = 3;
//    private CacheStorage cacheStorage = null;

    /**
     * Creates a new instance of BIApplication
     */
    public BIApplication() {
	// Specifies where to look for the {@link org.glassfish.jersey.server.model.Resource}
	packages("com.merc.webservice.rest.jersey.JAXRS_BISystem.Resources");
	
	/*
	 * Executes the scheduled database cache updating.
	 */
//	System.out.println("Starting cache initiation in 5 seconds");
//	cacheStorage = new CacheStorage(
//		DB_DESIGNDOC,
//		DB_VIEWNAMES,
//		UPDATE_FIRST_DELAY_SEC * 1000L,
//		(UPDATE_PERIOD_HR * 3600L) * 1000L);
    }
   
// Deprecated in place of Ektorp library
//    /**
//     * Wrapper function.
//     * Returning the current data in the cache storage.
//     * @return A list of {@link org.ektorp.ViewResult.Row}.
//     * @see {@link CacheStorage#getCurrentCache()}
//     */
//    public List<Row> getCurrentCacheStorage() {
//	return this.cacheStorage.getCurrentCache();
//    }
}
