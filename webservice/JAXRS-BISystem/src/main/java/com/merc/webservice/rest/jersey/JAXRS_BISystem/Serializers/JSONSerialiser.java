//package com.merc.webservice.rest.jersey.JAXRS_BISystem.Serializers;
//
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.Iterator;
//import java.util.Map;
//
//import org.ektorp.ViewResult.Row;
//
//import com.fasterxml.jackson.core.JsonParser;
//import com.fasterxml.jackson.databind.JsonDeserializer;
//import com.fasterxml.jackson.databind.JsonNode;
//
///**
// * JSON serialiser for {@link org.ektorp.ViewResult.Row} objects. Serialising is
// * handled internally as {@link com.fasterxml.jackson.databind.JsonNode}
// * objects.
// * 
// * Created: 2013-11-17. Modified: 2013-11-17.
// * 
// * @author Robin Larsson
// * @version 0.5
// */
//public class JSONSerialiser {
//
//    /**
//     * Creates a new instance of the JSONDeserializer
//     */
//    public JSONSerialiser() {
//
//    }
//
//    /**
//     * Serialises {@link org.ektorp.ViewResult.Row} by returning JSON String
//     * values and supports filtering. Values can only be a String.
//     * 
//     * @param serialised
//     *            A list of {@link org.ektorp.ViewResult.Row}.
//     * @param filter
//     *            A list of {@code Entry<String, String>}.
//     * @return A JSON encoded String.
//     */
//    public static String filterSerialise(Iterable<Row> deserialised,
//	    Iterable<Map.Entry<String, String>> filter) {
//	StringBuilder stringTemp = new StringBuilder();
//	ArrayList<Map.Entry<String, String>> memberTemp = new ArrayList<Map.Entry<String, String>>();
//
//	for (Row row : deserialised) {
//	    /*
//	     * Getting all the JSON members in the JSON object
//	     */
//	    Iterator<Map.Entry<String, JsonNode>> iteratorTemp = row
//		    .getValueAsNode().fields();
//	    /*
//	     * 
//	     */
//	    for (Map.Entry<String, String> filtered : filter) {
//
//		Map.Entry<String, JsonNode> entryTemp = iteratorTemp.next();
//		String currentKey = entryTemp.getKey();
//		JsonNode currentValue = entryTemp.getValue();
//		if (iteratorTemp.hasNext() == true) {
//		    /*
//		     * Filtering away those key-value pairs that exists in
//		     * {@link filter}. Checking as well that the value is a
//		     * String.
//		     */
//		    if (currentValue.textValue() != filtered.getValue()
//			    && currentValue.isTextual()
//			    && currentKey != filtered.getKey()) {
//			
//		    }
//		}
//		else {
//		    while (iteratorTemp.hasNext()) {
//
//		    }
//		}
//	    }
//	}
//	return temp.toString();
//    }
//}
