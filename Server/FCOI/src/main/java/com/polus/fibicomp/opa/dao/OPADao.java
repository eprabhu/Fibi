package com.polus.fibicomp.opa.dao;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.opa.pojo.OPAReviewStatusType;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.opa.dto.OPAAssignAdminDto;
import com.polus.fibicomp.opa.dto.OPACommonDto;
import com.polus.fibicomp.opa.dto.OPADashboardRequestDto;
import com.polus.fibicomp.opa.dto.OPADashboardResponseDto;
import com.polus.fibicomp.opa.dto.OPASubmitDto;
import com.polus.fibicomp.opa.pojo.OPADisclosure;
import com.polus.fibicomp.opa.pojo.OPADisclosureStatusType;
import com.polus.fibicomp.opa.pojo.OPAFormBuilderDetails;
import com.polus.fibicomp.opa.pojo.OPAPersonType;

@Service
public interface OPADao {

	boolean canCreateOpaDisclosure(String personId);

	OPACommonDto createOpaDisclosure(String personId, String homeUnit);

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
	 * @param opaDisclosureStatuses
	 * @param dispositionStatus
	 * @param opaDisclosureId
	 * @return boolean true/false, true if exists else false
	 */
	boolean isOPAWithStatuses(List<String> opaDisclosureStatuses, String dispositionStatus, Integer opaDisclosureId);

	/**
	 * This method is used to check admin assigned to the disclosure
	 * @param opaDisclosureId
	 * @return boolean true/false, true if added else false
	 */
	boolean isAdminAssigned(Integer opaDisclosureId);

	/**
	 * This method is used for get opaDisclosure details
	 * @param opaDisclosureId
	 * @return
	 */
	public OPADisclosure getOPADisclosure(Integer opaDisclosureId);

	/**
	 *
	 * @param requestDto
	 * @return
	 */
	OPADashboardResponseDto getOPADashboard(OPADashboardRequestDto requestDto);

	/**
	 *
	 * @param requestDto
	 * @param isCount
	 * @return
	 */
	ResultSet getOPADashboardResultSet(OPADashboardRequestDto requestDto, boolean isCount) throws SQLException;

	/**
	 * This method is used for inserting details into opaformbuilderdetails table
	 * @param opaFormBuilderDetails
	 * @return
	 */
	OPAFormBuilderDetails saveOrUpdateOpaFormBuilderDetails(OPAFormBuilderDetails opaFormBuilderDetails);

	/**
	 * This method is used to get opaformbuilderdetails using opaDisclosureId
	 * @param opaDisclosureId
	 * @return
	 */
	List<OPAFormBuilderDetails> getOpaFormBuilderDetailsByOpaDisclosureId(Integer opaDisclosureId);

	/**
	 * This method is used to get admin person id of an opa disclosure using opaDisclosureId
	 * @param opaDisclosureId
	 * @return
	 */
	String getAssignedAdmin(Integer opaDisclosureId);

	/**
	 * This method is used to get active and pending OPA disclosures by personId
	 * @param personId
	 * @return
	 */
	List<OPADisclosure> getActiveAndPendingOpaDisclosure(String personId);

	/**
	 * This method updates the update details of a OPA Disclosure
	 * @param opaDisclosureId
	 * @param timesStamp
	 * @return
	 */
	Timestamp updateOPADisclosureUpDetails(Integer opaDisclosureId, Timestamp timesStamp);

	/**
	 * This method updates the OPA disclosure statuses and update details
	 * @param opaDisclosureId
	 * @param opaDisclosureStatusCode
	 * @param dispositionStatusCode
	 * @return
	 */
	void updateOPADisclosureStatuses(Integer opaDisclosureId, Timestamp timestamp, String opaDisclosureStatusCode, String dispositionStatusCode);

	/**
	 *
	 * @param statusTypeCode
	 * @return
	 */
	OPAReviewStatusType getOPADisclosureStatusType(String statusTypeCode);

	/**
	 * This method is used to fetch OPA person type lookup
	 * @return
	 */
	List<OPAPersonType> getOpaPersonType();

	/**
	 * This method is used to check if same admin is added
	 * @return
	 */
	boolean isSameAdminPersonOrGroupAdded(Integer adminGroupId, String adminPersonId, Integer opaDisclosureId);

	/**
	 * This method is used to check if admin is added
	 * @return
	 */
	boolean isAdminPersonOrGroupAdded(Integer opaDisclosureId);

}
