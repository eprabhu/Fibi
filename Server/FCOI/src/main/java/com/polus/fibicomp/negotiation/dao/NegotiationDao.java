package com.polus.fibicomp.negotiation.dao;

import java.util.HashMap;
import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.negotiation.dto.NegotiationDataBus;
import com.polus.fibicomp.negotiation.dto.NegotiationProjectDetailsDto;
import com.polus.fibicomp.negotiation.dto.NegotiationReportDto;
import com.polus.fibicomp.negotiation.pojo.NegotiationsActivity;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAgreementValue;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationDetails;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAttachment;
import com.polus.fibicomp.negotiation.pojo.NegotiationsPersonnel;
import com.polus.fibicomp.negotiation.vo.LastLocationDetails;
import com.polus.fibicomp.negotiation.vo.NegotiationVO;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.vo.OrganizationSearchResult;
import com.polus.fibicomp.vo.SponsorSearchResult;

@Service
public interface NegotiationDao {

	/**
	 * this method is used for add negotiation activity
	 * 
	 * @param negotiationActivity
	 * @return
	 */
	public String addNegotiationActivity(NegotiationsActivity negotiationActivity);

	/**
	 * This method is used to view Negotiation.
	 * 
	 * @param negotiationVO - negotiationVO
	 * @return it returns details of Negotiation.
	 */

	public NegotiationVO loadNegotiation(NegotiationVO negotiationVO);

	/**
	 * This method is used to get details of Negotiations Location History by Id.
	 * 
	 * @param negotiationId
	 * @return list of location history.
	 */

	public NegotiationVO getNegotiationsLocationHistory(Integer negotiationId);

	/**
	 * This method is used to get last negotiation location details.
	 * 
	 * @param negotiationId
	 * @return it returns last negotiation location details.
	 */

	public LastLocationDetails getLastLocationDetails(Integer negotiationId);

	/**
	 * This method is used to add the negotiation Agreement value period.
	 * 
	 * @param negotiationsAgreementValue
	 * @return it returns success.
	 */

	public NegotiationsAgreementValue addAgreementValuePeriod(NegotiationsAgreementValue negotiationsAgreementValue);

	/**
	 * This method is used to add a new Person.
	 * 
	 * @param negotiationPersonnel
	 * @return it returns Success.
	 */

	public NegotiationsPersonnel addPerson(NegotiationsPersonnel negotiationPersonnel);

	/**
	 * This method is used to delete a period for the negotiation Agreement value.
	 * 
	 * @param negotiationsAgreementValue
	 * @return it returns Success .
	 */

	public String deleteAgreementValuePeriod(NegotiationsAgreementValue negotiationsAgreementValue);

	/**
	 * This method is used to delete a person.
	 * 
	 * @param person
	 * @return it returns Success .
	 */
	public String deletePersonnel(NegotiationsPersonnel negotiationsPersonnel);

	/**
	 * this method is used for add association details for the none case
	 * 
	 * @param associationDetails
	 * @return success message
	 */
	public String addNegotiationsAssociationDetails(NegotiationsAssociationDetails associationDetails);

	/**
	 * this method is used for get all sponsors list
	 * 
	 * @return list of sponsors
	 */
	public List<SponsorSearchResult> findSponsor();

	/**
	 * this method is used for get all organization sub award list
	 * 
	 * @return list of organizationSubAward
	 */
	public List<Organization> organizationSubAward();

	/**
	 * 
	 * @param negotiationsId
	 * @return
	 */
	public List<NegotiationProjectDetailsDto> getDetailsFromProjectId(NegotiationVO vo);

	/**
	 * 
	 * @param attachmentId
	 * @return
	 */
	public NegotiationsAttachment deleteAttachment(Integer attachmentId);

	/**
	 * this method is used for get template data for report generation
	 * @param negotiationDataBus
	 * @return
	 */
	byte[] getTemplateData(NegotiationDataBus negotiationDataBus);

	/**
	 * this method is used for fetch data for report
	 * @param negotiationDataBus
	 * @return
	 */
	NegotiationReportDto fetchNegotiationsData(NegotiationDataBus negotiationDataBus);

	/**
	 * this method is used for merge data from db to corresponding place holders of document
	 * @param outputDataFormat
	 * @param data
	 * @param negotiationReportDto
	 * @return
	 */
	public byte[] mergePlaceHolders(String outputDataFormat, byte[] data, NegotiationReportDto negotiationReportDto);

	/**
	 * this method is used for get all details corresponding the award id or ip 
	 * @param moduleKey
	 * @param moduleNumber
	 * @return
	 */
	public HashMap<String, Object> getProjectDetailsSP(String moduleKey, String moduleNumber);

	/**
	 * this method is used for get all negotiation association data
	 * @param negotiationId
	 * @return
	 */
	public List<NegotiationsAssociation> fetchAssociationData(Integer negotiationId);

	/**
	 * findSubawardOrganisations 
	 * @param searchString
	 * @return
	 */
	public List<OrganizationSearchResult> findSubawardOrganisations(String searchString);

	/**
	 * This method is used to fetch all SubAwardOrganization by negotiationId
	 * @param negotiationId
	 * @return
	 */
	public String getSubAwardOrganizationName(Integer negotiationId);

	/**
	 * This method is used to deleteNegotiationAssociationDetailsByAssociationId
	 * @param negotiationsAssociationId
	 * @return String
	 */
	public String deleteNegotiationAssociationDetailsByAssociationId(Integer negotiationsAssociationId);

	/**
	 * This method is used to getDetailsFromProjectId.
	 * @param vo
	 * @return NegotiationProjectDetailsDto
	 */
	public NegotiationProjectDetailsDto getDetailsFromProjectId(NegotiationsAssociation vo);
}
