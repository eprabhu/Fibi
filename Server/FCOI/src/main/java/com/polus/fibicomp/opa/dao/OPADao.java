package com.polus.fibicomp.opa.dao;

import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;

@Service
public interface OPADao {

	boolean isOpaDisclosureRequired(String personId);

	void createOpaDisclosure(String personId, String homeUnit);

	/**
	 * This method is used patch the opa disclosure for submit
	 * @param opaSubmitDto
	 * @return update timestamp
	 */
    Timestamp submitOPADisclosure(OPASubmitDto opaSubmitDto);

	/**
	 * This method is used to return or withdraw OPA Disclosure
	 * @param opaStatusCode
	 * @param opaDisclosureId
	 * @return
	 */
	Timestamp returnOrWithdrawOPADisclosure(String opaStatusCode, Integer opaDisclosureId);

	/**
	 * This method is used to update OPA disclosure admin and admin group
	 * @param assignAdminDto
	 * @return
	 */
	Timestamp assignAdminOPADisclosure(OPAAssignAdminDto assignAdminDto);

	Timestamp completeOPADisclosure(Integer opaDisclosureId);

	/**
	 * This method is used to check the disclosure is with statues @params
	 * @param opaDisclosureStatus
	 * @param dispositionStatus
	 * @param opaDisclosureId
	 * @return boolean true/false, true if exists else false
	 */
	boolean isOPAWithStatuses(String opaDisclosureStatus, String dispositionStatus, Integer opaDisclosureId);

	/**
	 * This method is used to check admin assigned to the disclosure
	 * @param opaDisclosureId
	 * @return boolean true/false, true if added else false
	 */
	boolean isAdminAssigned(Integer opaDisclosureId);
}
