package com.merc.webservice.rest.jersey.JAXRS_BISystem.Utilities;

/**
 * Miscellaneous functions that is needed.
 * @author Robin Larsson
 * @version 0.5
 */
public class Utilites {

    public Utilites() {
	
    }
    
    /**
     * Gets a new Epoch time, with the {@code secDifference} added.
     * @param start The start Epoch time in milliseconds.
     * @param secDifference The difference, in seconds, to add.
     * @return A new Epoch timestamp with the difference added.
     */
    public static long getEpochTimeOfAddedDifference(long start, long secDifference) {
	return start + (secDifference - (719528*24*3600) * 1000);
    }

}
