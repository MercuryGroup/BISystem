package com.merc.webservice.rest.jersey.JAXRS_BISystem.Application;

import org.glassfish.jersey.server.ResourceConfig;

/**
 * @author Robin Larsson
 * @version 0.5
 */
public class BIApplication extends ResourceConfig {

    /**
     * 
     */
    public BIApplication() {
	packages("com.merc.webservice.rest.jersey.JAXRS_BISystem.Resource");
    }

}
