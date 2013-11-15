package com.merc.webservice.rest.jersey.JAXRS_BISystem.Application;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.ArrayList;
import java.util.TimeZone;

import org.glassfish.jersey.server.ResourceConfig;

import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers.CacheHandler;
import com.merc.webservice.rest.jersey.JAXRS_BISystem.Handlers.DatabaseHandler;

/**
 * {@link javax.ws.rs.core.Application} class for JAXRS-BISystem.
 * Implements Jerseys' {@link org.glassfish.jersey.server.ResourceConfig}.
 * @author Robin Larsson
 * @version 0.5
 */
public class BIApplication extends ResourceConfig {
    private final String DB_DESIGNDOC = "_design/bi";
    private final List<String> DB_VIEWNAMES = new ArrayList<String>(
	    Arrays.asList("nyse", "nyse_market", "omx", "omx_market", "lse",
		    "lse_market"));
    private final int UPDATE_PERIOD_HR = 3;

    /**
     * Creates a new instance of BIApplication
     */
    public BIApplication() {
	// Specifies where to look for the {@link org.glassfish.jersey.server.model.Resource}
	packages("com.merc.webservice.rest.jersey.JAXRS_BISystem.Resources");
	
	/*
	 * Executes the scheduled database cache updating.
	 */
	System.out.println("Executing the cache, updating...");
	CacheHandler cacheHandler = new CacheHandler(
		DB_DESIGNDOC,
		DB_VIEWNAMES,
		0L,
		(UPDATE_PERIOD_HR * 3600) * 1000L);
	System.out.println("Cache updating done!");
    }
}
