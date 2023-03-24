package com.polus.fibicomp.proposal.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.compilance.vo.ProtocolVO;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.pojo.SpecialReviewApprovalType;
import com.polus.fibicomp.proposal.pojo.CongressionalDistrict;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalComment;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.vo.MaintainProjectTeamVO;
import com.polus.fibicomp.proposal.vo.ProposalPersonRoleVO;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "proposalService")
public interface ProposalService {

	/**
	 * This method is used to create proposal.
	 * @param vo - Object of ProposalVO class.
	 * @return Set of values to create proposal.
	 */
	public String createProposal(ProposalVO vo);

	/**
	 * This method is used to add attachments for a proposal.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @return A String of details of proposal data with list of attachments.
	 */
	public String addProposalAttachment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to save or update proposal.
	 * @param vo - Object of ProposalVO class.
	 * @return A string of details of a proposal.
	 */
	public String saveOrUpdateProposal(ProposalVO vo);

	/**
	 * This method is used to load proposal based on id.
	 * @param proposalId - Id of the proposal.
	 * @param personId - currentUser.
	 * @param userName 
	 * @param isProposalComparison 
	 * @return A string of details of a proposal.
	 */
	public String loadProposalById(Integer proposalId, String personId, String userName, Boolean isProposalComparison);

	/**
	 * This method is used to delete keywords from proposal.
	 * @param vo - Object of ProposalVO class. 
	 * @return a String of details of proposal with updated list of keywords.
	 */
	public String deleteProposalKeyword(ProposalVO vo);

	/**
	 * This method is used to delete area of research from proposal.
	 * @param vo - Object of ProposalVO class. 
	 * @return a String of details of proposal with updated list of area of research.
	 */
	public String deleteProposalResearchArea(ProposalVO vo);

	/**
	 * This method is used to delete proposal person from proposal.
	 * @param vo - Object of ProposalVO class. 
	 * @return a String of details of proposal with updated list of persons.
	 */
	public String deleteProposalPerson(ProposalVO vo);

	/**
	 * This method is used to delete declared sponsors from proposal.
	 * @param vo - Object of ProposalVO class. 
	 * @return a String of details of proposal with updated list of sponsors.
	 */
	public String deleteProposalSponsor(ProposalVO vo);

	/**
	 * This method is used to delete declared protocols from proposal.
	 * @param vo - Object of ProposalVO class. 
	 * @return a String of details of proposal with updated list of protocols.
	 */
	public String deleteIrbProtocol(ProposalVO vo);

	/**
	 * This method is used to delete attachment from proposal.
	 * @param vo - Object of ProposalVO class. 
	 * @return a String of details of proposal with updated list of attachments.
	 */
	public String deleteProposalAttachment(ProposalVO vo);

	/**
	 * This method is used to download proposal attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadProposalAttachment(Integer attachmentId);

	/**
	 * This method is used to submit proposal.
	 * @param vo - Object of ProposalVO class.
	 * @return a String of details of proposal.
	 */
	public String submitProposal(ProposalVO proposalVO);

	/**
	 * This method is used to approve or disapprove a proposal.
	 * @param files - Files need to be attached.
	 * @param formDataJSON - Request object data.
	 * @return a String of details of proposal.
	 */
	public String approveOrRejectProposal(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to delete special review of a proposal.
	 * @param proposalVO - Object of ProposalVO class.
	 * @return a String of details of proposal.
	 */
	public String deleteProposalSpecialReview(ProposalVO proposalVO);

	/**
	 * This method is used to load initial proposal data.
	 * @param vo - Object of ProposalVO class.
	 * @return a String of details of proposal.
	 */
	public void loadInitialData(ProposalVO proposalVO);

	/**
	 * This method is used to send attachment complete notification to PI.
	 * @param vo - Object of ProposalVO class.
	 * @return a String of details of proposal.
	 */
	public String sendAttachPINotification(ProposalVO proposalVO);

	/**
	 * This method is used to send attachments completed notification to final approver.
	 * @param vo - Object of ProposalVO class.
	 * @return a String of details of proposal.
	 */
	public String sendAttachApproverNotification(ProposalVO proposalVO);

	/**
	 * This method is used to delete proposal based on id.
	 * @param proposalId - Id of the proposal.
	 * @return success message.
	 */
	public String deleteProposal(ProposalVO vo);

	/**
	 * 
	 * @param proposalProjectTeam
	 * @return
	 */
	public String maintainProjectTeam(MaintainProjectTeamVO vo);
	/**
	 * This method is used to add and delete person roles
	 * @param ProposalVO 
	 * @return A string of details of a proposal person role.
	 */
	public String maintainProposalPersonRoles(ProposalPersonRoleVO proposalPersonRoleVO);
	/**
	 * This method is used detach proposal person roles based on proposalId.
	 * @param ProposalVO - Id of the proposal.
	 * @return A string of details of a proposal person roles.
	 */
	public String fetchProposalPersonRoles(ProposalPersonRoleVO proposalPersonRoleVO);

	/**
	 * This method is used to Add Science Keyword.
	 * @param vo - Object of ProposalVO class. 
	 * @return a String of details of proposal
	 */
	public String addScienceKeyword(ProposalVO vo);

	/**
	 * This method is used to Create entire proposal data inside a folder .
	 * @param response - HttpServletResponse.
	 * @param vo - proposalVO
	 * @return A String of details of proposal data.
	 */
	public void printEntireProposal(ProposalVO vo, HttpServletResponse response);

	/**
	 * This method is used to sort a proposal attachments.
	 * @param vo - Object of ProposalVO class.
	 * @return vo object to sorted proposal attachments.
	 */
	public String fetchSortedAttachments(ProposalVO vo);

	/**
	 * This method is used to Fund the Proposal .
	 * @param ProposalVO - vo.
	 * @return A String of details of proposal data.
	 */
	public String fundTheProposal(ProposalVO proposalVO);

	/**
	 * This method is used to Fund the Proposal .
	 * @param CommonVO - vo.
	 * @return A String of details of proposal data based on flag.
	 */
	public String saveProposalRankFromDashboard(CommonVO vo);

	/**
	 * This method is used to Fetch Activity type .
	 * @param ProposalVO - vo.
	 * @return A String of details of proposal data.
	 */
	public String fetchActivityType(ProposalVO vo);

	/**
	 * This method is used for get all activity types based on grant call type
	 * @return
	 */
	public String getAllActivityForGrantType();

	/**
	 * This method is used to add attachments for a proposal person.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @return A String of details of proposal data with list of attachments.
	 */
	public String addProposalPersonAttachment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to download proposal person attachment.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadProposalPersonAttachment(Integer attachmentId);

	/**
	 * This method is used to save or update Keyperson.
	 * @param vo - Object of ProposalVO class.
	 * @return A string of details of a Keyperson.
	 */
	public String saveOrUpdateKeyPerson(ProposalVO vo);

	/**
	 * This method is used to save or update ProjectTeam.
	 * @param vo - Object of ProposalVO class.
	 * @return A string of details of a ProjectTeam.
	 */
	public String saveOrUpdateProjectTeam(ProposalVO vo);

	/**
	 * This method is used to save or update SpecialReview.
	 * @param vo - Object of ProposalVO class.
	 * @return A string of details of a SpecialReview.
	 */
	public String saveOrUpdateSpecialReview(ProposalVO vo);

	/**
	 * This method is used to save or update Funds.
	 * @param vo - Object of ProposalVO class.
	 * @return A string of details of a Funds.
	 */
	public String saveOrUpdateFunds(ProposalVO vo);

	/**
	 * This method is used to save or update AreaOfResearch or Socetial challenge.
	 * @param vo - Object of ProposalVO class.
	 * @return A string of details of a proposal.
	 */
	public String saveOrUpdateAreaOfResearch(ProposalVO vo);

	/**
	 * This method is used to Fetch proposal Attachments.
	 * @param proposalId - id of Proposal.
	 * @param proposalStatusCode - id of Proposal Status.
	 * @return A string of details of a proposalAttachments.
	 */
	public String loadProposalAttachment(Integer proposalId, Integer proposalStatusCode);

	/**
	 * This method is used to save Grant call from proposal.
	 * @param proposalId - id of Proposal.
	 * @return A string of details of a proposalAttachments.
	 */
	public String saveGrantCallFromProposal(ProposalVO vo);

	/**
	 * This method is used to save.
	 * @param proposalId - id of Proposal.
	 * @return A string of details of a proposalAttachments.
	 */
	public String saveDescriptionOfProposal(ProposalVO vo);

	/**
	 * This method is used to Fetch EvaluationDetails.
	 * @param proposalId - id of Proposal.
	 * @return A string of details of a EvaluationDetails.
	 */
	public String loadEvaluationDetails(Integer proposalId, String logginPersonId, String loginPersonUnitNumber);

	/**
	 * This method is used to update AttachmentDescription.
	 * @param vo - object of ProposalVO.
	 * @return A string of details of a proposalAttachments.
	 */
	public String updateAttachmentDetails(ProposalVO vo);

	/**
	 * This method is used to exportSelectedAttachments inside a zip folder .
	 * @param vo- ProposalVO.
	 * @param response - HttpServletResponse.
	 */
	public void exportSelectedAttachments(ProposalVO vo, HttpServletResponse response);

	/**
	 * This method is used to load proposal home page data.
	 * @param vo- ProposalVO.
	 * @param response - void.
	 */
	public void loadProposalHomeData(ProposalVO proposalVO);

	/**
	 * This method is used to load proposal update, create and submitted user full name.
	 * @param proposal- Proposal.
	 * @param response - void.
	 */
	public void loadProposalUserFullNames(Proposal proposal);

	/**
	 * This method is used for the notification for proposal
	 * @param vo
	 * @return email success
	 */
	public String proposalInvitation(EmailServiceVO vo);

	/**
	 * This method is used for sending notification for proposal
	 * @param vo
	 * @param notificationTypeId
	 * @param dynamicEmailrecipients
	 * @return proposalVO object
	 */
	public ProposalVO sendProposalNotification(ProposalVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailrecipients);

	/**
	 * This method is used to update all attachment status
	 * @param vo - object of ProposalVO.
	 * @return response - A string of details of Proposal Attachments.
	 */
	public String updateAllAttachmentStatus(ProposalVO vo);

	/**
	 * This method is used to update proposal status
	 * @param vo - object of ProposalVO.
	 * @return response - HttpServletResponse.
	 */
	public String updateProposalStatusAsInactive(ProposalVO vo);

	/* This method is used to save or update proposal mile stone.
	 * @param vo - Object of ProposalVO class.
	 * @return A string of details of a proposal mile stone.
	 */
	public String saveOrUpdateProposalMileStone(ProposalVO vo);

	/**
	 * This method is used to delete mile stone of a proposal.
	 * @param proposalVO - Object of ProposalVO class.
	 * @return a String of details of proposal.
	 */
	public String deleteProposalMileStone(ProposalVO proposalVO);

	/**
	 * This method is used to save or update proposal call KPIs.
	 * 
	 * @param vo - Object of ProposalVO class.
	 * @return set of values to figure out details about a grant call KPIs.
	 */
	public String saveOrUpdateProposalKPI(ProposalVO vo);

	/**
	 * This method is used to save proposal call KPIs.
	 * 
	 * @param proposalId - id of Proposal.
	 * @param grantCallId - id of GrantCall.
	 * @param update User - update User.
	 * @return set of values to figure out details about a grant call KPIs.
	 */
	public void saveKPIFromGrantCall(Integer proposalId, Integer grantCallId, String updateUser);

	/**
	 * This method is used to save proposal call KPIs.
	 * 
	 * @param vo - Object of ProposalVO.
	 * @return SUCCESS message.
	 */
	public String assignAggregatorRoleToPI(ProposalVO vo);
   
	/**
	 * This method is used to fetch the details of fetchScoringCriteriaByProposal
	 * 
	 * @param vo
	 * @return object of ProposalVO as string
	 */
	public String fetchScoringCriteriaByProposal(ProposalVO proposalVO);
    
	/**
	 * This method is used to download workflow score  attachment.
	 * @param workflowReviewerAttmntsId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadWorkflowReviewerAttachment(Integer workflowReviewerAttmntsId);

	/**
	 * This method is used to fetch evaluation panels from GrantCallEvaluationpanel to ProposalEvaluationPanel.
	 * @param vo - Object of ProposalVO class. 
	 * @return
	 */
	public void fetchAndSaveProposalEvaluationPanels(ProposalVO vo);

	/**
	 * This method is used to check user can take Routing action.
	 * @param proposalVO - object of proposalVO
	 */
	public void canTakeRoutingAction(ProposalVO proposalVO);
	/**
	 * This method is used to save or update proposal comments.
	 * @param files - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @return String object of ProposalVO
	 */
	public String saveOrUpdateProposalComment(MultipartFile[] files, String formDataJson);

	/**
	 * This method is used to download proposal comment attachment by id.
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadProposalCommentAttachment(Integer attachmentId);

	/**
	 * This method is used to fetch a proposal comments.
	 * @param vo - Object of ProposalVO.
	 * @return it returns proposalComment list.
	 */
	public String fetchProposalComments(ProposalVO vo);

	/**
	 * This method is used to save proposal person roles.
	 * @param personId - id of person.
 	 * @param proposalId - id of proposal.
 	 * @param updateUser - update user.
 	 * @param roleId - id of role.
	 */
	public void saveProposalPersonRole(String personId, Integer proposalId, String updateUser, Integer roleId);

	/**
	 * This method is used to unlink the proposal from grantCall.
	 * @param vo - proposalVO object with proposalId
	 * @return proposal Details
	 */
	public String unlinkGrantCallFromProposal(ProposalVO vo);

	/**
	 * This method is used to add proposal attachment when waf enabled
	 * @param file
	 * @param formDataJson
	 * @return  A String of details of proposal data with list of attachments.
	 */
	public String addProposalAttachmentForWaf(ProposalVO vo);

	/**
	 * This method is used to add attachments for a proposal person for Waf.
	 * @param vo - object of ProposalVO.
	 * @return A String of details of proposal data with list of attachments.
	 */
	public String addProposalPersonAttachmentForWaf(ProposalVO vo);

	/**
	 * This method is used to assign permissions for proposal creator
	 * @param proposal
	 * @param derivedRoles
	 */
	public void assignDerivedRolesForCreator(Proposal proposal, List<ModuleDerivedRoles> derivedRoles);

	/**
	 * This method is used for assign permission for proposal PI
	 * @param proposalPerson
	 * @param proposalId
	 */
	public void assignDerivedRolesForPI(ProposalPerson proposalPerson, Integer proposalId);

	/**
	 * This method is used to delete the workflow reviewer comment by comment id
	 * @param workflowReviewerCommentsId
	 * @return success /failure
	 */
	public String deleteWorkflowScoreComments(Integer workflowReviewerCommentsId);

	/**
	 * This method is used to delete the work flow reviewer attachment
	 * @param workflowReviewerAttmntsId
	 * @return success /failure
	 */
	public String deleteWorkflowReviewerAttachment(Integer workflowReviewerAttmntsId);

	/**
	 * This method is used to save the workflow score details and to perform the endorse proposal
	 * @param formDataJson
	 * @param request
	 * @return details of proposal
	 */
	public String saveOrUpdateWorkflowScoreDetails(String formDataJson, MultipartHttpServletRequest request);

	/**
	 * @param files
	 * @param formDataJson
	 * @return
	 */
	public String withdrawProposal(MultipartFile[] files, String formDataJson);
	
	/**
	 * @param proposalVo
	 * @return updated proposalOrganization
	 */
	public String saveOrUpdateProposalOrganization(ProposalVO proposalVo);

	/**
	 * @param proposalVo
	 * @return updated Organization
	 */
	public String saveOrUpdateOrganization(ProposalVO proposalVo);

	/**
	 * @param organizationId
	 * @return Organization
	 */
	public String loadOrganizationDetails(String organizationId);

	/**
	 * @param vo
	 * @return updated cong district
	 */
	public String saveOrUpdateCongDistrict(ProposalVO vo);

	/**
	 * @param proposalOrganizationId
	 * @return map
	 */
	public String deleteProposalOrganization(Integer proposalOrganizationId);

	/**
	 * @param proposalCongDistrictId
	 * @return map
	 */
	public String deleteProposalCongDistrict(Integer proposalCongDistrictId);

	/**
	 * @param searchString
	 * @return CongressionalDistrict
	 */
	public List<CongressionalDistrict> findCongressionalDistricts(String searchString);
	
	/**
	 * 
	 * @param vo
	 * @return
	 */
	public String checkProposalRights(ProposalVO vo);

	/**
	 * @param this method is used to import proposal from template
	 * @param files
	 * @param formDataJson
	 */
	public ProposalVO importProposalTemplate(MultipartFile[] files, String formDataJson);

	/**
	 * @param this method is used to fetch proposal Key personnel attachments.
	 * @param proposalVO
	 */
	public String loadProposalKeyPersonAttachments(ProposalVO proposalVO);

	/**
	 * This method is used to exportSelectedAttachments inside a zip folder .
	 * @param vo- ProposalVO.
	 * @param response - HttpServletResponse.
	 */
	public void exportProposalPersonAttachments(ProposalVO vo, HttpServletResponse response);

	/**
	 * @param proposalId
	 * @param httpStatus 
	 * @return
	 * @throws Exception 
	 */
	public ResponseEntity<String> createProposalAdminCorrection(Integer proposalId);

	/**
	 * @param vo
	 * @return
	 */
	public String showProposalHistory(Integer proposalId);

	/**
	 * @param proposalId
	 * @return
	 */
	public ResponseEntity<String> completeProposalAdminCorrection(Integer proposalId);

	/**
	 * This method used to fetch all key personnel attachment type
	 * @return
	 */
	String loadProposalKeyPersonnelAttachmentTypes();

	/**
	 * This method used to fetch all key personnel attachment uploaded persons
	 * @param proposalId
	 * @return
	 */
	String loadProposalKeyPersonnelPersons(Integer proposalId);

	/**
	 * This method is used to delete key personnel attachment
	 * @param proposalVO
	 * @return
	 */
	String deleteKeyPersonnelAttachment(ProposalVO proposalVO);

	/**
	 * This method is used to upload key personal attachments
	 * @param files
	 * @param formDataJSON
	 * @return
	 */
	String uploadProposalPersonAttachment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to update proposal key personal attachment
	 * @param formDataJson
	 * @return
	 */
    String updateKeyPersonnelAttachment(String formDataJson);
    
    /**
	 * @param proposalVO
	 * @return will update proposal person certification  
	 */
	public String updateProposalPersonCertification(ProposalVO proposalVO);

	/**
	 * @param proposalId
	 * @return list of proposal persons for certification
	 */
	public String proposalPersonsForCertification(ProposalVO proposalVO);

	/**										  
	 * @param proposalVO
	 * @return boolean
	 */
	public String sendPersonCertificationMail(ProposalVO proposalVO);

	/**
	 * This method is used to check if the login user has the right to delet proposal.
	 * @param proposalVO.
	 * @return .
	 */
	public String canDeleteProposal(ProposalVO proposalVO);

	/**
	 * This method is used to load all protocol details based on criteria.
	 * @param ProtocolVO - vo.
	 * @return 
	 */
	public String loadProtocolDetail(ProtocolVO vo);

	/**
	 * This method is used to fetch protocol details based on protocol number.
	 * @param ProtocolVO - vo.
	 * @return .
	 */

	public IrbProtocol getIrbProtocols(String protocolNumber);

	/**
	 * This method is used to get Ac protocol detail based on protocol number.
	 * @param protocolNumber.
	 * @return .
	 */
	public AcProtocol getAcProtocols(String protocolNumber);

	/**
	 * This method is used to prepare integrated protocol details
	 * @param proposalSpecialReview.
	 * @param persons
	 * @param nonEmployees
	 * @return .
	 */
	public ProposalSpecialReview setIntegratedProposalSpecilReviewDetail(Map<String, String> persons, Map<String, String> nonEmployees, ProposalSpecialReview proposalSpecialReview);

	/**
	 * This method is used to set approval type for Irb protocol
	 * @param irbProtocol.
	 * @return .
	 */
	public SpecialReviewApprovalType setApprovalTypeForIrbProtocol(IrbProtocol irbProtocol);

	/**
	 * This method is used to set approval type for Ac protocol
	 * @param acProtocol.
	 * @return .
	 */
	public SpecialReviewApprovalType setApprovalTypeForAcProtocol(AcProtocol acProtocol);

	/**
	 * This method is used to set full name
	 * @param persons.
	 * @param personId
	 * @param isNonEmployee
	 * @return .
	 */
	public String setFullNameForProtocol(Map<String, String> persons, String personId, Boolean isNonEmployee);

	/**
	 * This method is used to prepare proposal comment
	 * @param proposalId 
	 * @return list of ProposalComment.
	 */
	public List<ProposalComment> prepareProposalComment(Integer proposalId);

	/**
	 * This method is used to add degree against a person in proposal.
	 * @param vo 
	 * @return list of person degree.
	 */
	public String addProposalPersonDegree(ProposalVO vo);

	/**
	 * This method is used to get all degree of a person in a proposal.
	 * @param vo 
	 * @return list of person degree.
	 */
	public String getPersonDegree(ProposalVO vo);


	/**
	 * This method is used to delete degree of a person in a proposal.
	 * @param  proposalPersonDegreeId
	 */
	public String deleteProposalPersonDegree(Integer proposalPersonDegreeId);

}
