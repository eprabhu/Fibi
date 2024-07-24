package com.polus.integration.dao;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service
public interface IntegrationDao {

	/**
	 * This method is used to convert Object into JSON format.
	 * 
	 * @param object - request object.
	 * @return response - JSON data.
	 */
	public String convertObjectToJSON(Object object);

}
