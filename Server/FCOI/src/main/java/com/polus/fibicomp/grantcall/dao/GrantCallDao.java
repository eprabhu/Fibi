package com.polus.fibicomp.grantcall.dao;

import java.util.HashMap;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.evaluation.pojo.GrantCallEvaluationPanel;
import com.polus.fibicomp.grantcall.pojo.FundingSchemeAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallActionLog;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachType;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCallCriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibility;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibilityType;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIQuestionnaire;
import com.polus.fibicomp.grantcall.pojo.GrantCallKPI;
import com.polus.fibicomp.grantcall.pojo.GrantCallKeyword;
import com.polus.fibicomp.grantcall.pojo.GrantCallRelevant;
import com.polus.fibicomp.grantcall.pojo.GrantCallScoringCriteria;
import com.polus.fibicomp.grantcall.pojo.GrantCallStatus;
import com.polus.fibicomp.grantcall.pojo.GrantCallType;
import com.polus.fibicomp.grantcall.pojo.GrantEligibiltyTargetType;
import com.polus.fibicomp.pojo.RelevantField;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorFundingScheme;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;

@Transactional
@Service
public interface GrantCallDao {

	/**
	 * This method is used to fetch all grant call types.
	 * 
	 * @return A list of grant call types.
	 */
	public List<GrantCallType> fetchAllGrantCallTypes();

	/**
	 * This method is used to fetch all grant call status.
	 * 
	 * @return A list of grant call status.
	 */
	public List<GrantCallStatus> fetchAllGrantCallStatus();

	/**
	 * This method is used to fetch all science keywords.
	 * 
	 * @return A list of science keywords.
	 */
	public List<ScienceKeyword> fetchAllScienceKeywords();

	/**
	 * This method is used to fetch all sponsor types.
	 * 
	 * @return A list of sponsor types.
	 */
	public List<SponsorType> fetchAllSponsorTypes();

	/**
	 * This method is used to fetch sponsors based on sponsor type.
	 * @param searchString - searchString
	 * @param sponsorTypeCode - type code of sponsor.
	 * @return A list of sponsors corresponding to the type.
	 */
	public List<Sponsor> fetchSponsorsBySponsorType(String searchString, String sponsorTypeCode);

	/**
	 * This method is used to fetch all grant call criteria.
	 * 
	 * @return A list of grant call criteria.
	 */
	public List<GrantCallCriteria> fetchAllGrantCallCriteria();

	/**
	 * This method is used to fetch all eligibility types.
	 * 
	 * @return A list of eligibility types.
	 */
	public List<GrantCallEligibilityType> fetchAllEligibilityTypes();

	/**
	 * This method is used to fetch all grant call attachment types.
	 * 
	 * @return A list of grant call attachment types.
	 */
	public List<GrantCallAttachType> fetchAllGrantCallAttachTypes();

	/**
	 * This method is used to fetch grant call by Id.
	 * 
	 * @param grantCallId - Id of the grant call.
	 * @return An object of grant call.
	 */
	public GrantCall fetchGrantCallById(Integer grantCallId);

	/**
	 * This method is used to save or update grant call.
	 * 
	 * @param grantCall - Object of GrantCall.
	 * @return set of values to figure out details about a grant call.
	 */
	public GrantCall saveOrUpdateGrantCall(GrantCall grantCall);

	/**
	 * This method is used to fetch grant call status by status code.
	 * 
	 * @param grantStatusCode - Status code of grant call status.
	 * @return An object of grant call status.
	 */
	public GrantCallStatus fetchStatusByStatusCode(Integer grantStatusCode);

	/**
	 * This method is used to fetch attachment by Id.
	 * 
	 * @param attachmentId - Id of the attachment.
	 * @return an object of GrantCallAttachment.
	 */
	public GrantCallAttachment fetchAttachmentById(Integer attachmentId);

	/**
	 * This method is used to fetch grantCallType by grantcallTypeCode.
	 * 
	 * @param grantTypeCode - Id of the grantCallType.
	 * @return an object of GrantCallType.
	 */
	public GrantCallType fetchGrantCallTypeByGrantTypeCode(Integer grantTypeCode);

	/**
	 * This method is used to delete GrantCall based on id.
	 * 
	 * @param GrantcallId - Id of the GrantCall.
	 * @return success message.
	 */
	public String deleteGrantCall(Integer grantCallId);

	/**
	 * This method is used fetch Proposal sBy GrantCallId
	 * @return Proposal Types Proposal.
	 */
	public List<Proposal> fetchProposalsByGrantCallId(Integer grantCallId);

	/**
	 * This method is used to fetch FundingScheme based on sponsor.
	 * @param sponsorTypeCode - type code of sponsorCode.
	 * @return A list of sponsors corresponding to the type.
	 */
	public List<SponsorFundingScheme> fetchFundingSchemeBySponsor(String sponsorCode);

	/**
	 * This method is used to fetch FundingScheme Attachments based on FundingScheme.
	 * @param fundingSouceTypeCode - type code of FundingScheme.
	 * @return A list of FundingSchemeAttachment corresponding to the type.
	 */
	public List<FundingSchemeAttachment> fetchFundingSchemeAttachmentBasedOnScheme(Integer fundingSchemeId);

	/**
	 * This method is used to fetch attachment by Id.
	 * 
	 * @param attachmentId - Id of the attachment.
	 * @return an object of FundingSchemeAttachment.
	 */
	public FundingSchemeAttachment fetchFundingSchemeAttachmentById(Integer attachmentId);
	
	public String getMaxGrantCallKeyword();

	public GrantCallKeyword saveOrUpdateGrantCallKeyword(GrantCallKeyword grantCallKeyword);

	/**
	 * This method is used to fetch all Relevant fields
	 * @return list of relevant fields
	 */
	public List<RelevantField> fetchAllRelevantFields();

	/**
	 * This method is used to fetch all Relevant fields based on grant call id
	 * @param grantCallId - grantCallId
	 * @return list of relevant fields
	 */
	public List<GrantCallRelevant> getGrantCallRelevantFieldsByGrantCallId(Integer grantCallId);

	public GrantCall fetchGrantCallDetails(Integer grantCallId);

	public List<GrantEligibiltyTargetType> fetchAllGrantEligibiltyTargetTypes();

	/**
	 * This method is used to save or update IOIQuestionnaire based on grantCallId.
	 * @param grantQuestionnaire - GrantCallIOIQuestionnaire of the GrantCall.
	 * @return An object of GrantCallIOIQuestionnaire.
	 */
	public GrantCallIOIQuestionnaire saveOrUpdateGrantCallIOIQuestionnaire(GrantCallIOIQuestionnaire grantQuestionnaire);

	/**
	 * This method is used to fetch IOIQuestionnaire based on grantCallId.
	 * @param grantCallId - grantCallId of the GrantCall.
	 * @return An object of GrantCallIOIQuestionnaire.
	 */
	public GrantCallIOIQuestionnaire fetchGrantCallIOIQuestionnaireByGrantId(Integer grantCallId);

	/**
	 * This method is used to delete a GrantCallIOIQuestionnaire.
	 * @param grantIOIQuestionnaireId - grantIOIQuestionnaireId of GrantCallIOIQuestionnaire
	 * @return
	 */
	public void deleteGrantIOIQuestionnaire(Integer grantIOIQuestionnaireId);

	/**
	 * This method is used to get all eligibility target persons 
	 * @param moduleItemKey
	 * @return list of personIds
	 */
	public List<HashMap<String, Object>> fetchEligibilityTargetPersons(Integer moduleItemKey);

	/**
	 * This method is used to delete a GrantCallKPI.
	 * @param grantKPIId - GrantCallKPI of GrantCall
	 * @return
	 */
	public GrantCallKPI deleteGrantCallKPI(GrantCallKPI grantCallKPI);

	/**
	 * This method is used to delete a GrantCallScoringCriteria.
	 * @param grantCallScoringCriteria - GrantCallScoringCriteria of GrantCall
	 * @return
	 */
	public GrantCallScoringCriteria deleteGrantCallScoringCriteria(GrantCallScoringCriteria grantCallScoringCriteria);

	/**
	 * This method is used to add scroing in the main panel for proposal evaluation.
	 * 
	 * @return success message.
	 */
	public ProposalEvaluationScore saveOrUpdateProposalEvalautionScore(ProposalEvaluationScore proposalEvaluationScore);
	/**
	 * This method is used to delete a GrantCallEvaluationPanel.
	 * @param grantCallEvaluationPanel - GrantCallEvaluationPanel of GrantCall
	 * @return
	 */
	public GrantCallEvaluationPanel deleteGrantCallEvaluationPanel(GrantCallEvaluationPanel grantCallEvaluationPanel);
	
	/**
	 * This method is used to fetch proposal Evaluation by grant Id.
	 * 
	 * @param grantCallId - Id of the grant call.
	 * @return An object of grant call.
	 */
	
	public List<ProposalEvaluationScore> fetchProposalEvaluationById(Integer grantCallId);

	/**
	 * This method is used to fetch the grant category code by grant type code
	 * @param grantTypeCode
	 * @return grantCategoryCode
	 */
	public Integer fetchGrantCategoryCodeByGrantTypeCode(Integer grantTypeCode);

	/**
	 * This method is used to save or update grant call eligibilty.
	 * 
	 * @param grantCalleligibilty - Object of GrantCallEligibility.
	 * @return set of values to figure out details about a grant call eligibilty.
	 */
	public GrantCallEligibility saveOrUpdateGrantCallEligibility(GrantCallEligibility grantCalleligibilty);

	/**
	 * This method is used to get the grant-call name by grant call id
	 * @param grantCallId
	 * @return grant-call name
	 */
	public String getGrantCallNameByGrantId(Integer grantCallId);

	/**
	 * This method is used to get the grant-call name by grant call id
	 * @param grantCallId
	 * @return grant-call name
	 */
	public String getLeadUnitNumberByGrantId(Integer grantCallId);

	/**
	 * This method is used to get all sponsor funding schemes
	 * @return list of sponsor funding schemes
	 */
	public List<SponsorFundingScheme> fetchAllSponsorFundingSchemes();

	/**
	 * This method is used to save grant call action log details.
	 * @param grantHeaderId - id of grant call.
	 * @param grantCallActionTypeCode - grantCall ActionType Code
	 * @param updateUser -updateUser
	 * @return object of GrantCallActionLog.
	 */
	public GrantCallActionLog saveOrUpdateGrantCallActionLog(GrantCallActionLog grantCallActionLog);

	/**
	 * This method is used to get grant call action log details.
	 * @param vo - object of GrantCallVO.
	 * @return list of GrantCallActionLog.
	 */
	public List<GrantCallActionLog> fetchGrantCallActionLog(Integer grantCallId);

	/**
	 * This method is used to get grant call title.
	 * @param grantCallId .
	 * @return title of grant call.
	 */
	public String getGrantCallTitleByGrantId(Integer grantCallId);

	/**
	 * This method is used check if GrantCall is linked in any proposal or award
	 * @param grantCallId
	 */
	public Boolean checkGrantCallLinked(Integer grantCallId);

	/**
	 * This method is used check if GrantCall eligibility target external exist or not
	 * @param grantCallId
	 */
	public Boolean checkGrantEligibilityExternalExist(Integer grantCallId);

}
