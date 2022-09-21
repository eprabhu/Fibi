package com.polus.fibicomp.grantcall.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.grantcall.dto.ScoringReportDto;
import com.polus.fibicomp.grantcall.pojo.GrantCallActionLog;
import com.polus.fibicomp.grantcall.vo.EvaluationMainPanelVO;
import com.polus.fibicomp.grantcall.vo.GrantCallVO;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.pojo.Sponsor;

@Transactional
@Service
public interface GrantCallService {

	/**
	 * This method is used to create a grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return vo object to create grant call.
	 */
	public String createGrantCall(GrantCallVO vo);

	/**
	 * This method is used to load grant call by id.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return An object of grant call.
	 */
	public String loadGrantCallById(GrantCallVO vo);

	/**
	 * This method is used to save or update grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return set of values to figure out details about a grant call.
	 */
	public String saveUpdateGrantCall(GrantCallVO vo);

	/**
	 * This method is used to publish grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return set of values to figure out details about a grant call.
	 */
	public String publishGrantCall(GrantCallVO vo);

	/**
	 * This method is used to delete keywords from grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return a String of details of grant call with updated list of keywords.
	 */
	 public String deleteGrantCallKeyword(GrantCallVO vo);

	/**
	 * This method is used to delete point of contacts from grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return a String of details of grant call with updated list of grant call
	 *         contacts.
	 */
	public String deleteGrantCallContact(GrantCallVO vo);

	/**
	 * This method is used to delete area of research from grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return a String of details of grant call with updated list of area of
	 *         research.
	 */
	public String deleteGrantCallAreaOfResearch(GrantCallVO vo);

	/**
	 * This method is used to delete eligibility from grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return a String of details of grant call with updated list of grant call
	 *         eligibility.
	 */
	public String deleteGrantCallEligibility(GrantCallVO voo);

	/**
	 * This method is used to delete attachment from grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return a String of details of grant call with updated list of grant call
	 *         attachment.
	 */
	public String deleteGrantCallAttachment(GrantCallVO vo);

	/**
	 * This method is used to add grant call attachment.
	 * 
	 * @param files        - attached files.
	 * @param formDataJSON - form data for the attachment.
	 * @return a String of details of grant call data with list of attachments.
	 */
	public String addGrantCallAttachment(MultipartFile[] files, String formDataJSON);

	/**
	 * This method is used to download grant call attachment.
	 * 
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadGrantCallAttachment(Integer attachmentId);

	/**
	 * This method is used to make a copy of Grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return a String of details of Grant call.
	 */
	public String copyGrantCall(GrantCallVO vo);

	/**
	 * This method is used to delete Eligible Departments from grant call.
	 * 
	 * @param vo - Object of GrantCallVO class.
	 * @return a String of details of grant call with updated list of keywords.
	 */
	public String deleteGrantEligibleDepartments(GrantCallVO vo);

	/**
	 * This method is used to delete GrantCall based on id.
	 * 
	 * @param GrantCallId - Id of the GrantCall.
	 * @return success message.
	 */
	public String deleteGrantCall(GrantCallVO vo);

	/**
	 * This method is used to fetch fetchSponsorsBySponsorType based on sponsor.
	 * @param searchString -- input string.
	 * @param sponsorTypeCode - sponsorTypeCode
	 * @return A list of sponsors.
	 */
	public List<Sponsor> fetchSponsorsBySponsorType(String searchString, String sponsorTypeCode);

	/**
	 * This method is used to get Societal Challenge Area based on search string.
	 * 
	 * @param searchString - input string.
	 * @return a list of Societal Challenge Area.
	 */
	
	public String fetchFundingSchemeBySponsor(GrantCallVO vo);

	/**
	 * This method is used to Archive the GrantCall.
	 * @param vo - Object of GrantCallVO class.
	 * @return String object of vo.
	 */
	public String archiveGrantCall(GrantCallVO vo);

	/**
	 * This method is used for the notification for grant call
	 * @param vo
	 * @return email success
	 */
	public String grantInvitation(EmailServiceVO emailServiceVO);

	/**
	 * This method is used to download Funding Scheme Attachment.
	 * 
	 * @param attachmentId - Id of the attachment to download.
	 * @return attachmentData.
	 */
	public ResponseEntity<byte[]> downloadFundingSchemeAttachment(Integer attachmentId);

	/**
	 * This method is used to add new grantcall keyword.
	 * @param vo - Object of GrantCallVO class.
	 * @return String object of vo.
	 */
	public String addGrantCallKeyword(GrantCallVO vo);

	public String deleteGrantCallRelevantField(GrantCallVO vo);

	/**
	 * This method is used to send grant call notifications
	 * @param vo
	 * @param notificationTypeId
	 * @param dynamicEmailrecipients
	 * @return String object of GrantCallVO
	 */
	public GrantCallVO sendGrantCallNotification(GrantCallVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailrecipients);

	/**
	 * This method is used while linking a questionnaire to a GrantCall
	 * @param Object of GrantCallVO class.
	 * @return String object of GrantCallVO
	 */
	public String saveOrUpdateGrantCallIOIQuestionnaire(GrantCallVO vo);

	/**
	 * This method is used to delete questionnaire to a GrantCall
	 * @param Object of GrantCallVO class.
	 * @return String object of GrantCallVO
	 */
	public String deleteGrantIOIQuestionnaire(GrantCallVO vo);

	/**
	 * This method is used to fetch questionnaire of a GrantCall
	 * @param Object of GrantCallVO class.
	 * @return String object of GrantCallVO
	 */
	public String fetchGrantCallIOIQuestionnaireByGrantId(GrantCallVO vo);

	/**
	 * This method is used to save the main-panel scoring results
	 * @param vo
	 * @return String object of EvaluationMainPanelVO
	 */
	public String saveOrUpdateProposalEvalautionScore(EvaluationMainPanelVO vo);

	/**
	 * This method is used to get all proposals by grantCallId
	 * @param vo
	 * @return String object of EvaluationMainPanelVO
	 */
	public String getProposalByGrantCallId(EvaluationMainPanelVO vo);

	/**
	 * This method is used to get the criteria details by proposal id
	 * @param proposalId
	 * @param loginPersonUnitNumber 
	 * @param loginPersonId 
	 * @param loginUserName 
	 * @return score details by criteria or person
	 */
	public Map<String, List<ScoringReportDto>> getCriteriaScoreByProposalId(Integer proposalId, String loginPersonId, String loginPersonUnitNumber, String loginUserName, Boolean byCriteria);

	/**
	 * This method is used to get the criteria details by proposal id
	 * @param proposalId
	 * @param loginPersonId
	 * @param loginPersonUnitNumber
	 * @param loginUserName
	 * @return score details by criteria
	 */
	public String getCriteriaScoreByProposalId(Integer proposalId, String loginPersonId, String loginPersonUnitNumber, String loginUserName);
   
	/**
     * This method is used to save Proposal Evaluation Rank.
     * @param vo - Object of EvaluationMainPanelVO.
     * @return String object of EvaluationMainPanelVO.
     */
    public String updateProposalEvalautionRank(EvaluationMainPanelVO vo);

    /**
    * This method is used to save Area Of Research.
    * @param vo - Object of GrantCallVO.
    * @return String object of GrantCallVO.
    */
    public String saveOrUpdateAreaOfResearch(GrantCallVO vo);

    /**
     * This method is used to save Multiple proposal in the main panel.
     * @param vo - Object of EvaluationMainPanelVO.
     * @return String object of EvaluationMainPanelVO.
     */
    public String saveOrUpdateProposalEvalautionScores(EvaluationMainPanelVO vo);

    /**
     * This method is used to save Point of Contact.
     * @param vo - Object of GrantCallVO.
     * @return String Object of GrantCallVO.
     */
	public String saveOrUpdatePointOfContact(GrantCallVO vo);

    /**
     * This method is used to save grant call eligibility separately.
     * @param vo - Object of GrantCallVO.
     * @return String object of GrantCallVO.
     */
    public String saveGrantCallEligibilityCriteria(GrantCallVO vo);

	/**
	 * This method is used to update AttachmentDescription.
	 * @param vo - object of GrantCallVO.
	 * @return A string of details of a proposalAttachments.
	 */
	public String updateGrantCallAttachmentDetails(GrantCallVO vo);

	/**
	 * This method is used to add grant call attachment for Waf enabled.
	 * @param grantCallVO - Data for the attachment.
	 * @return a String of details of grant call data with list of attachments.
	 */
	public String addGrantCallAttachmentForWaf(GrantCallVO grantCallVO);

	/**
	 * This method is used to save grant call action log details.
	 * @param grantHeaderId - id of grant call.
	 * @param grantCallActionTypeCode - grantCall ActionType Code
	 * @param updateUser -updateUser
	 * @return object of GrantCallActionLog.
	 */
	public GrantCallActionLog saveGrantCallActionLogDetails(Integer grantHeaderId, String grantCallActionTypeCode, String updateUser);

	/**
	 * This method is used to get grant call action log details.
	 * @param vo - object of GrantCallVO.
	 * @return list of GrantCallActionLog.
	 */
	public String getGrantCallHistory(GrantCallVO vo);

}
