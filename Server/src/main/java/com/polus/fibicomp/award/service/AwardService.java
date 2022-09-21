package com.polus.fibicomp.award.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.Report;
import com.polus.fibicomp.award.vo.AwardAttachmentsVO;
import com.polus.fibicomp.award.vo.AwardCostShareVO;
import com.polus.fibicomp.award.vo.AwardDatesandAmountVO;
import com.polus.fibicomp.award.vo.AwardKPIVO;
import com.polus.fibicomp.award.vo.AwardLinkInstituteProposalVO;
import com.polus.fibicomp.award.vo.AwardMilestoneVO;
import com.polus.fibicomp.award.vo.AwardPaymentAndInvoicesVO;
import com.polus.fibicomp.award.vo.AwardSpecialReviewVO;
import com.polus.fibicomp.award.vo.AwardSubContractVO;
import com.polus.fibicomp.award.vo.AwardSummaryVO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.award.vo.MaintainAwardHierarchyVO;
import com.polus.fibicomp.award.vo.PersonnelVO;
import com.polus.fibicomp.award.vo.ReportTermsVO;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;

/**
 * @author sajith.m
 *
 */
@Transactional
@Service
public interface AwardService {

	/**
	 * This method is used to retrieve award data for award view.
	 * 
	 * @param awardId - unique identifier for award
	 * @return set of values to figure out details about an award
	 * @throws Exception
	 */
	public String getAwardSummaryData(String awardId) throws Exception;

	/**
	 * This method is used to retrieve award hierarchy data.
	 * 
	 * @param awardNumber
	 * @return set of values to figure out hierarchy details about an award
	 * @throws Exception
	 */
	public String getAwardHierarchyData(String awardNumber, String selectedAwardNumber) throws Exception;

	/**
	 * This method is used to save Award General Details.
	 * 
	 * @param awardVo - Object of AwardVO.
	 * @return .
	 * @throws Exception
	 */
	public String saveAwardDetails(AwardVO awardVo) throws Exception;

	/**
	 * @param vo AwardVO object for personId and awardId
	 * @return all look up data as String, for initial loading of award module
	 * @throws Exception
	 */
	public String getAwardLookupData(AwardVO vo) throws Exception;

	/**
	 * @param searchString is the search keyword
	 * @return a list of organizations matching the search keyword
	 */
	public List<Organization> findOrganizationList(String searchString);

	/**
	 * This method is to get the general details
	 * 
	 * @param awardVo
	 * @return
	 * @throws Exception
	 */
	public String getAwardDetails(AwardVO awardVo) throws Exception;

	/**
	 * This method is to save or update award special review
	 * 
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public String saveAwardSpecialReview(AwardSpecialReviewVO vo) throws Exception;

	/**
	 * This method is to save or update award cost share
	 * 
	 * @param costShareVO
	 * @return
	 * @throws Exception
	 */
	public String saveAwardCostShare(AwardCostShareVO costShareVO) throws Exception;

	/**
	 * This method is to save Award Subcontracts
	 * 
	 * @param subContractVO
	 * @return
	 * @throws Exception
	 */
	public String saveAwardSubContracts(AwardSubContractVO subContractVO) throws Exception;

	/**
	 * This method is to remove a special review entry
	 * 
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public String deleteAwardSpecialReview(AwardSpecialReviewVO vo) throws Exception;

	/**
	 * This method is to remove the award sub contract
	 * 
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public String deleteAwardSubContract(AwardSubContractVO vo) throws Exception;

	/**
	 * This method is used to remove Cost Share from an award
	 * 
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public String deleteAwardCostShare(AwardCostShareVO vo) throws Exception;

	/**
	 * This method is to make the award final
	 * 
	 * @param awardVo
	 * @return
	 * @throws Exception
	 */
	public String saveAwardFinal(AwardVO awardVo) throws Exception;

	public String maintainSpecialApproval(ReportTermsVO reportTermsVo) throws Exception;

	public String getReportLookupData(ReportTermsVO awardVo) throws Exception;

	public String getTermsLookupData(ReportTermsVO awardVo) throws Exception;

	public String maintainReports(ReportTermsVO reportTermsVo) throws Exception;

	public String maintainTerms(ReportTermsVO reportTermsVo) throws Exception;

	public String getReportsData(ReportTermsVO reportTermsVo) throws Exception;

	public String getTermsData(ReportTermsVO reportTermsVo) throws Exception;

	public String linkInstituteProposalToAward(AwardLinkInstituteProposalVO awardLinkInstituteProposalVO) throws Exception;

	public String generateNextAwardNumber();

	public String addAwardAttachments(MultipartFile[] files, String formDataJSON);

	public String getReportTrackingDetails(ReportTermsVO reportTermsVo) throws Exception;

	public String saveReportTrackingDetails(ReportTermsVO reportTermsVo) throws Exception;

	public String getAttachmentDetails(AwardAttachmentsVO attachmentVo) throws Exception;

	public String deleteAwardAttachment(AwardAttachmentsVO attachmentVo) throws Exception;

	public ResponseEntity<byte[]> downloadAwardAttachment(Integer awardAttachmentId);

	public String deleteAwardTransaction(AwardDatesandAmountVO vo) throws Exception;

	/**
	 * this method is used to get all data in award hierarchy
	 * 
	 * @param awardNumber
	 * @param selectedAwardNumber
	 * @return
	 * @throws Exception
	 */
	public String getAwardHierarchyDataList(String awardNumber, String selectedAwardNumber) throws Exception;

	/**
	 * this method is used for add new award in hierarchy
	 * 
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public String saveAwardHierarchyData(MaintainAwardHierarchyVO VO,AwardVO awardVO) throws Exception;

	/**
	 * This method is used to save or update Key Personnel detail.
	 * @param personnelVO - Object of PersonnelVO.
	 * @return success
	 */
	public String saveOrUpdateKeyPersonnel(PersonnelVO personnelVO);

	/**
	 * This method is used to delete Key Personnel detail.
	 * @param personnelVO - Object of PersonnelVO.
	 * @return success
	 */
	public String deleteKeyPersonnel(PersonnelVO personnelVO);

	/**
	 * This method is used to save or update project team detail.
	 * @param personnelVO - Object of PersonnelVO.
	 * @return success
	 */
	public String saveOrUpdateAwardProjectTeam(PersonnelVO personnelVO);

	/**
	 * This method is used to delete project team detail.
	 * @param personnelVO- Object of PersonnelVO.
	 * @return success
	 */
	public String deleteAwardProjectTeam(PersonnelVO personnelVO);

	/**
	 * This method is used to save or update award contacts
	 * @param personnelVO - Object of PersonnelVO.
	 * @return success
	 */
	public String saveOrUpdateAwardContact(PersonnelVO personnelVO);

	/**
	 * This method is used to delete award contact
	 * @param personnelVO - Object of PersonnelVO
	 * @return
	 */
	public String deleteAwardContact(PersonnelVO personnelVO);

	/**
	 * This method is used to delete the award keyword
	 * @param vo - Object of AwardVO
	 * @return
	 */
	public String deleteAwardKeyword(AwardVO vo);

	/**
	 * This method is used to delete award person unit
	 * @param vo - Object of PersonnelVO
	 * @return
	 */
	public String deleteAwardPersonUnit(PersonnelVO vo);

	/**
	 * This meythod is used to save the award person attachments
	 * @param files
	 * @param formDataJsonR
	 * @return object of proposalPersonattachments
	 */
	public String addAwardPersonAttachment(MultipartFile[] files, String formDataJson);

	/**
	 * This method is used to download the award person attachment
	 * @param attachmentid
	 * @return
	 */
	public ResponseEntity<byte[]> downloadAwardPersonAttachment(Integer attachmentid);

	/**
	 * This method is canTakeRoutingAction
	 * @param attachmentid
	 * @return
	 */
	public void canTakeRoutingAction(AwardVO awardVO);

	/**
	 * This method is used to send award notification.
	 * @param awardVO
	 * @param notificationTypeId
	 * @param dynamicEmailrecipients
	 * @return awardVO
	 */
	public AwardVO sendAwardNotification(AwardVO awardVO, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailrecipients);

	public void createAwardByFundingProposal(AwardLinkInstituteProposalVO awardLinkInstituteProposalVO) throws Exception;

	/**
	 * This method is used to show all award history
	 * @param awardNumber
	 * @return list of awards
	 */
	public String showAwardHistory(AwardVO vo);

	/**
	 * This method is used detach award person roles based on awardId.
	 * @param personnelVO - Id of the awardId.
	 * @return A string of details of a award person roles.
	 */
	public String fetchAwardPersonRoles(PersonnelVO personnelVO);

	/**
	 * This method is used to add  person roles
	 * @param PersonnelVO 
	 * @return A string of details of a award person role.
	 */
	public String maintainAwardPersonRoles(PersonnelVO personnelVO);

	/**
	 * This method is used to delete person roles
	 * @param PersonnelVO 
	 * @return A string of details of a award person role.
	 */
	public String deleteAwardPersonRoles(PersonnelVO personnelVO);

	/**
	 * This method is used to add award report tracking attachment
	 * @param files
	 * @param formDataJson
	 * @return AwardReportTrackingFile
	 */
	public String addAwardReportTrackingAttachment(MultipartFile[] files, String formDataJson);

	/**
	 * This method is used to download award report tracking attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadAwardReportTrackingAttachment(Integer awardReportTrackingFileId);

	/**
	 * This method is used to delete award report tracking attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public String deleteAwardReportTrackingAttachment(AwardVO vo);

	/**
	 * This method is used to withdrawAward.
	 * @param files
	 * @param formDataJson
	 * @return String data.
	 */
	public AwardVO withdrawAward(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to prepare award contact list.
	 * @param awardId - awardId.
	 * @return award contact list.
	 */
	public List<AwardContact> prepareAwardContactList(Integer awardId);

	/**
	 * This method is used to update award attachment details.
	 * @param vo - awardVO object.
	 * @return award attachment list.
	 */
	public String updateAwardAttachmentDetails(AwardVO vo);

	/**
	 * This method is used to fetch Report By ReportClass.
	 * @param reportClassCode - reportClassCode.
	 * @return List of Report.
	 */
	public List<Report> fetchReportByReportClass(String reportClassCode);

	/**
	 * This method is used to save Service Request From Award.
	 * @param vo - ServiceRequestVO object.
	 * @return String Data of Service Request.
	 */
	public String saveServiceRequestFromAward(ServiceRequestVO vo);

	/**
	 * This method is used for the notification for award
	 * @param vo
	 * @return email success
	 */
	public String awardInvitation(EmailServiceVO emailServiceVO);

	/**
	 * This method is used to sent Award Remainders.
	 */
	public void sentAwardRemainders();

	/**
	 * This method is used to save Award Payment And Invoices.
	 * @param vo - object of AwardPaymentAndInvoicesVO
	 * @return String Data of Award
	 */
	public String saveAwardPaymentAndInvoices(AwardPaymentAndInvoicesVO vo);

	/**
	 * This method is used to get Award Payment And Invoices.
	 * @return String Data of AwardPaymentAndInvoicesVO
	 */
	public String getAwardPaymentAndInvoices();

	/**
	 * This method is used to save or update award milestone details.
	 * @param vo - AwardMilestoneVO object.
	 * @return award milestone list.
	 */
	public String saveOrUpdateAwardMilestone(AwardMilestoneVO vo);

	/**
	 * This method is to remove a award milestone
	 * 
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public String deleteAwardMilestone(AwardMilestoneVO vo);

	/**
	 * This method is used to save and update Award KPIs
	 * @param awardKPIVO - AwardKPIVO object.
	 * @return object of AwardKPIVO.
	 */
	public String saveOrUpdateAwardKPI(AwardKPIVO awardKPIVO);

	/**
	 * This method is used to delete Award KPI.
	 * @param awardKPIVO - AwardKPIVO object.
	 */
	public String deleteAwardKPI(AwardKPIVO awardKPIVO);

	/**
	 * This method is used to export All Award Attachments inside a zip folder .
	 * @param vo- AwardVO.
	 * @param response - HttpServletResponse.
	 */
	public void exportAllAwardAttachments(AwardVO vo, HttpServletResponse response);

	/**
	 * This method is used to add data to award hierarchy
	 * @param award
	 */
	public void moveDataToAwardHierarchy(Award award);

	/**
	 * This method is used to save or update AreaOfResearch or Socetial challenge.
	 * @param vo - Object of AawardVO class.
	 * @return A string of details of a award.
	 */
	public String saveOrUpdateAwardAreaOfResearch(AwardVO vo);

	/**
	 * This method is used to delete area of research from award.
	 * @param vo - Object of AwardVO class. 
	 * @return a String of details of award with updated list of area of research.
	 */
	public String deleteAwardResearchArea(AwardVO vo);

	/**
	 * This method is used to save.
	 * @param awardVO - object of AwardVO.
	 * @return A string of details of a Award.
	 */
	public String saveDescriptionOfAward(AwardVO vo);

	/**
	 * This method is used to unlink the grantCall from award.
	 * @param awardVO - AwardVO object with awardId
	 * @return awardVO
	 */
	public String unlinkGrantCallFromAward(AwardVO awardVO);

	/**
	 * This method is used to save award Status.
	 * @param awardVO - object of AwardVO.
	 * @return A string of details of a Award.
	 */
	public String saveAwardWorkflowStatusForSponsor(AwardVO awardVO);

	/**
	 * This method is used to add award person attachment for WAF
	 * @param vo
	 * @return list of award person attachment
	 */
	public String addAwardPersonAttachmentForwaf(AwardVO vo);

	/**
	 * This method is used to add award attachment for WAF
	 * @param awardAttachmentsVO
	 * @return list of award attachments
	 */
	public String addAwardAttachmentsForwaf(AwardAttachmentsVO awardAttachmentsVO);

	/**
	 * This method is used to add award report tracking attachment for WAF
	 * @param awardVO
	 * @return list of report tracking attachment
	 */
	public String addAwardReportTrackingAttachmentForWaf(AwardVO awardVO);

	/**
	 * This method is used to update documentupdateUser and documentUpdateTimeStamp
	 * @param awardId
	 * @param updateUser
	 * @return 
	 */
	public void updateAwardDocumentUpdateUserAndTimestamp(Integer awardId, String updateUser);

	/**
	 * This method is used to withdrawAward.
	 * @param vo - object of AwardVO.
	 * @return String data.
	 */
	public String withdrawAwardForWaf(AwardVO vo);

	/**
	 * This method is used to update the award budget status based on document type and editable section
	 * @param award
	 * @param budgetStatus
	 */
	public void updateAwardBudgetStatus(Award award, String budgetStatus);

	/**
	 * This method is used to update the award budget status by status code
	 * @param awardId
	 * @param budgetStatus
	 */
	public void updateAwardBudgetStatusByStatusCode(Integer awardId, String budgetStatus);

	/**
	 * This method is used to set the sap feed status to know whether we can create the variation based on the status.
	 * @param vo
	 */
	public void setSapFeedStatus(AwardVO vo);

	/**
	 * This method is used to assign permission for PI
	 * @param awardPerson
	 * @param awardId
	 */
	public void assignDerivedRolesForPI(AwardPerson awardPerson, Integer awardId);

	/**
	 * This method is used for assign permission for award creator
	 * @param award
	 * @param createPersonId
	 * @param derivedRoles
	 */
	public void assignDerivedRolesForCreator(Award award, String createPersonId, List<ModuleDerivedRoles> derivedRoles);

	/**
	 * This method is used to return active and pending awards
	 * @param vo
	 * @return object of AwardSummaryVO as string
	 */
	public String getAwardVersions(AwardSummaryVO vo);

	/**
	 * This method is used find can CreateVariationRequest.
	 * @param awardNumber
	 * @param variationTypeCode
	 * @return boolean value
	 */
	public void canCreateVariationRequest(AwardVO vo, String awardNumber, String variationTypeCode);

	/**
	 * This method is used find Pending Award Details.
	 * @param vo
	 * @param awardNumber
	 * @param awardId
	 */
	public void getPendingAwardDetails(AwardVO vo, String awardNumber, Integer awardId);

   /**
	 * This method is used to get award report tracking attachment versions
	 * @param awardReportTrackingId
	 * @return list of attachment else [] array
	 */
	public String getReportTrackingAttachmentVersions(Integer awardReportTrackingId);

	/**
	 * This method is used to update report terms
	 * @param reportTermsVo
	 * @return updated report terms
	 */
	public String saveOrUpdateReportTracking(ReportTermsVO reportTermsVo);

	/**
	 * This method is used to delete report terms
	 * @param awardReportTrackingId
	 * @return success message
	 */
	public String deleteReportTracking(Integer awardReportTrackingId);

	/**
	 * @param awardNumber
	 * @return new child award Number
	 */
	public String getBaseAwardNumber(String awardNumber);

	/**
	 * This method is used to search the given string to get result like elastic result
	 * @param searchString
	 * @return list of award
	 */
	public String findAward(String searchString);
	
	/**
	 * @param awardVO
	 * @return boolean
	 */
	public String updateReportTermsInAward(AwardVO awardVO);
  
	/* @param batchId
	 * @param awardNumber
	 */
	public void updateAwardBudgetStatusBasedOnBatchId(Integer batchId, String awardNumber, String budgetStatusCode, String currentBudgetStatusCode);

	/**
	 * @param awardNumber
	 * @return
	 */
	public String deleteAward(Integer awardId);
	
	/**
	 * This method is used for delete an Award
	 * @param awardNumber
	 * @return
	 */
    public String canDeleteAward(Integer awardId);
	
	/**
	 * This method is used for validation
	 * @param awardNumber
	 * @return
	 */
	public Set<Integer> getSubModuleCodeBasedOnAwardNumber(String awardNumber);

	/**
	 * This method is used for prepare special review data's based on integrated protocol
	 * @param awardSpecialReview
	 * @param persons
	 * @param nonEmployees
	 * @return
	 */
	public AwardSpecialReview setIntegratedAwardSpecialReviews(Map<String, String> persons, Map<String, String> nonEmployees, AwardSpecialReview awardSpecialReview);

	/**
	 * This method is used for getAwardPlaceholders
	 * @param awardVO
	 * @return
	 */
	public Map<String, String> getAwardPlaceholders(AwardVO awardVO);

}
