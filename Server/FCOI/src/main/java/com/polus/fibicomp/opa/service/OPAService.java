package com.polus.fibicomp.opa.service;

import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public interface OPAService {

	/**
	 * This service is used to check if OPA disclosure has to be submitted for the logged in user
	 * @param personId 
	 */
	Boolean canCreateOpaDisclosure(String personId);

	ResponseEntity<Object> createOpaDisclosure(String personId, String homeUnit);

	/**
	 * This method id used to submit opa disclosure
	 * @param opaSubmitDto
	 * @return
	 */
    ResponseEntity<Object> submitOPADisclosure(OPASubmitDto opaSubmitDto);

	/**
	 * This method is used to withdraw OPA disclosure
	 * @param opaDisclosureId
	 * @param opaDisclosureNumber
	 * @return
	 */
	ResponseEntity<Object> withdrawOPADisclosure(Integer opaDisclosureId, String opaDisclosureNumber);

	/**
	 * This method used to return OPA disclosure
	 * @param opaDisclosureId
	 * @param opaDisclosureNumber
	 * @return
	 */
	ResponseEntity<Object> returnOPADisclosure(Integer opaDisclosureId, String opaDisclosureNumber);

	/**
	 * This method is used to assign OPA Disclosure admin
	 * @param assignAdminDto
	 * @return
	 */
	ResponseEntity<Object> assignAdminOPADisclosure(OPAAssignAdminDto assignAdminDto);

	/**
	 * This method used to complete OPA disclosure
	 * @param opaDisclosureId
	 * @param opaDisclosureNumber
	 * @return
	 */
    ResponseEntity<Object> completeOPADisclosure(Integer opaDisclosureId, String opaDisclosureNumber);

	/**
	 * This method is used to reassign OPA Disclosure admin
	 * @param assignAdminDto
	 * @return
	 */
    ResponseEntity<Object> reassignAdminOPADisclosure(OPAAssignAdminDto assignAdminDto);

    /**
	 * This method is used to get header details of OPA disclosure
	 * @param opaDisclosureId
	 * @return
	 */
	ResponseEntity<Object> getOPADisclosure(Integer opaDisclosureId);

	/**
	 * This method is used to fetch reporter OPA Dashboard
	 * @param requestDto
	 * @return
	 */
    ResponseEntity<Object> getOPADashboard(OPADashboardRequestDto requestDto);
}
