package com.polus.fibicomp.opa.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public interface OPAService {

	/**
	 * This service is used to check if OPA disclosure has to be submitted for the logged in user
	 * @param personId 
	 * @param CreateOpaDto
	 */
	Boolean canCreateOpaDisclosure(String personId);

	ResponseEntity<Object> createOpaDisclosure(String personId, String homeUnit);

}
