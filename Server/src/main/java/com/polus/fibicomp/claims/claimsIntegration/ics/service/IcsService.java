package com.polus.fibicomp.claims.claimsIntegration.ics.service;

import org.springframework.stereotype.Service;

@Service(value = "icsService")
public interface IcsService {

	/**
	* This method is used to integrate claim student travel details
	* @param dateValue - Date Processed
	* @param endDate - end Date
	*/
	public void claimStudentTravelIntegration(String dateValue, String endDate);

}
