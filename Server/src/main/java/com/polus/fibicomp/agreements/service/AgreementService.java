package com.polus.fibicomp.agreements.service;

import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.agreements.dto.AgreementClausesGroup;
import com.polus.fibicomp.agreements.pojo.AgreementActionLog;
import com.polus.fibicomp.agreements.pojo.AgreementClauses;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;
import com.polus.fibicomp.agreements.pojo.ClausesBank;
import com.polus.fibicomp.agreements.vo.AgreementLinkModuleVO;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.agreements.vo.TemplateManagementVO;
import com.polus.fibicomp.negotiation.pojo.Negotiations;
import com.polus.fibicomp.negotiation.pojo.NegotiationsLocation;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Service
public interface AgreementService {

	/**
	 * This method is used to add agreement attachments.
	 * files - attachment files
	 * formDataJSON - formDataJSON
	 * @return agreement object.
	 */
	public String addAgreementAttachment(MultipartFile[] files, String formDataJSON, HttpServletResponse response);

	/**
	 * This method is used to create agreement.
	 * @param personId
	 * @return
	 */
	public String createAgreement(String personId);

	/**
	 * This method is used to load InitialData of agreement.
	 * @param agreementVO -  object of AgreementVO.
	 */
	public void loadInitialData(AgreementVO agreementVO);

	/**
	 * This method is used to save or update agreement.
	 * @param agreementVO - Object of AgreementVO class.
	 * @return A string of details of a agreement.
	 */
	public String saveOrUpdateAgreement(AgreementVO agreementVO);

	/**
	 * This method is used to view a particular Agreement by agreementId.
	 * 
	 * @param vo
	 * @return it returns the agreement details.
	 */
	public String loadAgreementById(AgreementVO vo);

	/**
	 * This method is used to deleteAgreementAttachment.
	 * @param vo - AgreementVO
	 * @return it returns the agreement details.
	 */
	public String deleteAgreementAttachment(AgreementVO vo);

	/**
	 * This method is used to downloadAgreementAttachment.
	 * @param attachmentid - attachment id
	 * @return reponse entity.
	 */
	public ResponseEntity<byte[]> downloadAgreementAttachment(Integer attachmentId, String exportType);

	/**
	 * This method is used to saveOrUpdateOrganisation.
	 * @param vo - object of AgreementVO
	 * @return String reponse.
	 */
	public String saveOrUpdateOrganisation(AgreementVO vo);

	/**
	 * This method is used to loadAgreement Attachments.
	 * @param agreementRequestId - agreement id
	 * @return String response.
	 */
	public String loadAgreementAttachments(AgreementVO vo);

	/**
	 * This method is used to submitAgreement.
	 * @param agreementVO - agreementVO.
	 * @return String reponse.
	 */
	public String submitAgreement(AgreementVO vo);

	/**
	 * This method is used to send notification for agreement.
	 * @param agreementHeader - agreementHeader.
	 * @param notificationTypeId - notificationTypeId
	 * @param dynamicEmailRecipients - dynamicEmailRecipients
	 */
	public void sendNotificationForAgreement(AgreementVO agreementVO, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients);

	/**
	 * This method is used to deleteAgreementSponsor.
	 * @param agreementSponsorId - id of AgreementSponsor.
	 * @return String reponse.
	 */
	public String deleteAgreementSponsor(Integer agreementSponsorId);

	/**
	 * This method is used to loadAgreementNegotiation.
	 * @param vo - object of AgreementVO.
	 * @return String reponse.
	 */
	public String loadAgreementNegotiation(AgreementVO vo);

	/**
	 * This method is used to add agreement template.
	 * files - attachment files
	 * formDataJSON - formDataJSON
	 * @return agreement object.
	 */
	public String addAgreementTemplate(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to save Agreement Person.
	 * @param vo -object of AgreementVO.
	 * @return agreement object.
	 */
	public String saveAgreementPerson(AgreementVO vo);

	/**
	 * This method is used to prepareNegotiationsLocations.
	 * @param negotiationsLocations - List of negotiationsLocations.
	 * @return String reponse.
	 */
	public List<NegotiationsLocation> prepareNegotiationsLocations(List<NegotiationsLocation> negotiationsLocations);

	/**
	 * This method is used to add Clauses Group in admin dashboard.
	 * @param vo - object of TemplateManagementVO.
	 * @return String reponse.
	 */
	public String addClausesGroup(TemplateManagementVO vo);

	/**
	 * This method is used to link ClausesGroup To AgreementType.
	 * @param vo - object of TemplateManagementVO.
	 * @return String reponse.
	 */
	public String linkClausesGroupToAgreementType(TemplateManagementVO vo);

	/**
	 * This method is used to load All Form PlaceHolders.
	 * @return String reponse.
	 */
	public String loadAllFormPlaceHolders();

	/**
	 * This method is used to load All Clauses.
	 * @return String reponse.
	 */
	public String loadAllClauses();

	/**
	 * This method is used to load All Templates.
	 * @param vo - object of TemplateManagementVO.
	 * @return String reponse.
	 */
	public String loadAllTemplates(TemplateManagementVO vo);

	/**
	 * This method is used to load All Clauses Templates.
	 * @param vo - object of TemplateManagementVO.
	 * @return String reponse.
	 */
	public String loadAllClausesTemplates(TemplateManagementVO vo);

	/**
	 * This method is used to deleteClauses.
	 * @param vo - object of TemplateManagementVO.
	 * @return String reponse.
	 */
	public String deleteClauses(TemplateManagementVO vo);

	/**
	 * This method is used to unlinkClausesGroupToAgreementType.
	 * @param vo - object of TemplateManagementVO.
	 * @return String reponse.
	 */
	public String unlinkClausesGroupToAgreementType(TemplateManagementVO vo);

	/**
	 * This method is used to downloadAgreementTemplate.
	 * @param attachmentid - id of AgreementTemplate.
	 * @return ResponseEntity.
	 */
	public ResponseEntity<byte[]> downloadAgreementTemplate(Integer attachmentid);

	/**
	 * This method is used to deleteAgreementTemplate.
	 * @param vo - object of TemplateManagementVO.
	 * @return String reponse.
	 */
	public String deleteAgreementTemplate(TemplateManagementVO vo);

	/**
	 * This method is used to get all questions in agreement module
	 * @param vo
	 * @return list of question
	 */
	public String loadAllQuestionsPlaceHolders(TemplateManagementVO vo);

	/**
	 * This method is used to get agreement history
	 * @param agreementRequestId - agreementRequestId.
	 * @return A agreement history details.
	 */
	public String getAgreementHistory(Integer agreementRequestId);

	/**
	 * This method is used to add action log entry
	 * @param agreementRequestId - agreementRequestId.
	 * @param actionTypeCode - actionTypeCode
	 * @param updateUser - updateUser
	 * @param message - message
	 * return AgreementActionLog object
	 */
	public AgreementActionLog addActionLogEntry(Integer agreementRequestId, String actionTypeCode, String updateUser, String message);

	/**
	 * This method is used to find Clauses
	 * @param searchString - searchString.
	 * @return A List of Clauses.
	 */
	public List<ClausesBank> findClauses(String searchString);

	/**
	 * This method is used to find Clauses
	 * @param searchString - searchString.
	 * @return A List of Clauses.
	 */
	public String saveOrUpdateAgreementClauses(AgreementVO vo);

	/**
	 * This method is used to delete Agreement Clauses
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String deleteAgreementClauses(AgreementVO vo);

	/**
	 * This method is used to load Agreement Clauses By AgreementId
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String loadAgreementClausesByAgreementId(AgreementVO vo);

	/**
	 * This method is used to ready To Execute
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String readyToExecute(AgreementVO vo);

	/**
	 * This method is used to finalize Agreement
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String finalizeAgreement(MultipartFile[] files, String formDataJSON, HttpServletResponse response);

	/**
	 * This method is used to add template in attachments.
	 * @param agreementVO - agreementVO
	 * @param mergedOutput - mergedOutput
	 */
	public void saveTemplateAsAttachments(AgreementVO agreementVO, byte[] mergedOutput);

	/**
	 * This method is used to submitAgreementWorkflow
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String submitAgreementReview(AgreementVO vo);

	/**
	 * This method is used to completeReview
	 * @param agreementRequestId - agreementRequestId.
	 * @return String response.
	 */
	public String completeReview(AgreementVO vo);

	/**
	 * This method is used to delete Agreement Group
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String deleteAgreementGroup(AgreementVO vo);

	/**
	 * This method is used to startReview
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String startReview(AgreementVO vo);

	/**
	 * This method is used to mark Attachment As Final
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String markAttachmentAsFinal(AgreementVO vo);

	/**
	 * This method is used to deleteAgreementSponsorContact
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String deleteAgreementSponsorContact(AgreementVO vo);

	/**
	 * This method is used to saveOrUpdateOrganisationContact
	 * @param vo - Object of AgreementVO.
	 * @return String response.
	 */
	public String saveOrUpdateOrganisationContact(AgreementVO vo);

	/**
	 * This method is used to addToClausesBank
	 * @param vo - Object of TemplateManagementVO.
	 * @return String response.
	 */
	public String addToClausesBank(TemplateManagementVO vo);

	/**
	 * This method is used to loadAllClausesBank
	 * @return String response.
	 */
	public String loadAllClausesBank();

	/**
	 * This method is used to deleteClausesById
	 * @param clauseCode 
	 * @return String response.
	 */
	public String deleteClausesById(Integer clauseCode);

	/**
	 * This method is used getAgreementSponsors
	 * @param agreementRequestId 
	 * @return List of AgreementSponsor.
	 */
	public List<AgreementSponsor> getAgreementSponsors(Integer agreementRequestId);

	/**
	 * This method is used prepareAgreementClauses
	 * @param List of agreementClauses 
	 * @return List of AgreementClausesGroup.
	 */
	public List<AgreementClausesGroup> prepareAgreementClauses(List<AgreementClauses> agreementClauses);

	/**
	 * This method is used loadPersonDetailsInAgreement
	 * @param agreementHeader - Object of agreementHeader
	 */
	public void loadPersonDetailsInAgreement(AgreementHeader agreementHeader);

	/**
	 * This method is used get Negotiation Id Based On AgreementId
	 * @param agreementRequestId - Object of agreementHeader
	 * @return - negotiationId
	 */
	public Integer getNegotiationIdBasedOnAgreementId(Integer agreementRequestId);

	/**
	 * This method is add Agreement Clauses Based  OnAgreementType
	 * @param agreementTypeCode - agreementTypeCode
	 * @param agreementRequestId - agreementRequestId
	 * @param updateUser - updateUser
	 * @return - List of AgreementClauses
	 */
	public List<AgreementClauses> addAgreementClausesBasedOnAgreementType(String agreementTypeCode, Integer agreementRequestId, String updateUser);

	/**
	 * This method is used createNewNegotiation
	 * @param agreementHeader - Object of agreementHeader
	 * @return - Negotiations object
	 */
	public Negotiations createNewNegotiation(AgreementHeader agreementHeader);

	/**
	 * This method is used saveAgreementPeople
	 * @param vo - Object of AgreementVO
	 * @return - String response .
	 */
	public String saveAgreementPeople(AgreementVO vo);

	/**
	 * This method is used agreementInvitation
	 * @param vo - Object of EmailServiceVO
	 * @return - String response.
	 */
	public String agreementInvitation(EmailServiceVO vo);

	/**
	 * This method is used preparePeopleDetails
	 * @param agreementPeoples - List of AgreementPeople
	 * @return - List of AgreementPeople.
	 */
	public List<AgreementPeople> preparePeopleDetails(List<AgreementPeople> agreementPeoples);

	/**
	 * This method is used deleteAgreementPeople
	 * @param vo - Object of AgreementVO
	 * @return - String response.
	 */
	public String deleteAgreementPeople(AgreementVO vo);

	/**
	 * This method is used to return agreement
	 * @param files - files.
	 * @param formDataJson - formDataJson
	 * @return String response.
	 */
	public String returnAgreement(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used deleteAgreementPeople
	 * @param agreementHeader - Object of agreementHeader
	 */
	public void loadAgreementUserFullNames(AgreementHeader agreementHeader);

	/**
	 * This method is used prepareAgreementTypeHistory
	 * @param agreementRequestId - Object of agreementHeader
	 * @return Set of string values
	 */
	public Set<String> prepareAgreementTypeHistory(Integer agreementRequestId);

	/**
	 * This method is used printEntireAgreement
	 * @param agreementRequestId - agreementRequestId.
	 * @param personId - personId.
	 * @param userName - userName.
	 * @return Set of string values
	 */
	public void printEntireAgreement(Integer agreementRequestId, HttpServletResponse response, String personId, String userName);

	/**
	 * This method is used assignAgreementAdmin
	 * @param vo - object of AgreementVO.
	 * @return String response
	 */
	public String assignAgreementAdmin(AgreementVO vo, boolean isEntryNeeded);

	/**
	 * This method is used terminateAgreement
	 * @param vo - object of AgreementVO.
	 * @return String response
	 */
	public String terminateAgreement(AgreementVO vo);

	/**
	 * This method is used abandonAgreement
	 * @param vo - object of AgreementVO.
	 * @return String response
	 */
	public String abandonAgreement(AgreementVO vo);

	/**
	 * This method is used transferAgreement
	 * @param vo - object of AgreementVO.
	 * @return String response
	 */
	public String transferAgreement(AgreementVO vo);

	/**
	 * This method is used reopenAgreement
	 * @param vo - object of AgreementVO.
	 * @return String response
	 */
	public String reopenAgreement(AgreementVO vo);

	/**
	 * This method is used updateAgreementStatus
	 * @param agreementHeader - object of agreementHeader.
	 * @param agreementStatusCode - Status code of agreement.
	 * @return String response
	 */
	public void updateAgreementStatus(AgreementHeader agreementHeader, String agreementStatusCode);

	/**
	 * This method is used updateAgreementStatus
	 * @param personId - personId.
	 * @param agreementVO - object of agreementVO.
	 * @return String response
	 */
	public void setAgreementHomeUnit(String personId, AgreementVO agreementVO);

	/**
	 * @param vo
	 * @return
	 */
	public String linkModuleToAgreement(AgreementLinkModuleVO vo);

	/**
	 * @param clauseGroupCode
	 * @return
	 */
	public String deleteClausesGroup(Integer clauseGroupCode);

	public String getPersonGroup(String personId);

	/**
	 * This method is used to update AttachmentDescription.
	 * @param vo - object of AgreementVO.
	 * @return A string of details of a agreementAttachments.
	 */
	public String updateAttachmentDetails(AgreementVO vo);
	
	/**
	 * This method is used to delete AgreementRecord.
	 * @param agreementRequestId
	 * @return
	 */
    String deleteAgreement(Integer agreementRequestId);
    
    /**
	 * @param agreementvo
	 */
	public void canAgreementTakeRoutingAction(AgreementVO agreementvo);

}
