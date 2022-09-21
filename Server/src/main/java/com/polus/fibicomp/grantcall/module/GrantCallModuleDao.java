package com.polus.fibicomp.grantcall.module;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.grantcall.pojo.GrantCallActionLog;
import com.polus.fibicomp.grantcall.pojo.GrantCallAttachment;
import com.polus.fibicomp.grantcall.pojo.GrantCallContact;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibility;
import com.polus.fibicomp.grantcall.pojo.GrantCallEligibleDepartment;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallRelevant;
import com.polus.fibicomp.grantcall.pojo.GrantCallResearchArea;
import com.polus.fibicomp.proposal.pojo.ProposalEvaluationScore;

@Transactional
@Service
public interface GrantCallModuleDao {

	/**
	 * This method is used to fetch GrantCall Attachment Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of GrantCallAttachment.
	 */
	public List<GrantCallAttachment> fetchGrantCallAttachmentBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdateProposalAttachment.
	 * @param grantCallAttachment - Object of the GrantCallAttachment.
	 * @return A Object of GrantCallAttachment.
	 */
	public GrantCallAttachment saveOrUpdateGrantCallAttachment(GrantCallAttachment grantCallAttachment);

	/**
	 * This method is used to delete GrantCall Attachment.
	 * @param grantCallAttachment - Object of the GrantCallAttachment.
	 * @return A Object of GrantCallAttachment.
	 */
	public GrantCallAttachment deleteGrantCallAttachment(GrantCallAttachment grantCallAttachment);

	/**
	 * This method is used to delete GrantCall Attachment.
	 * @param attachmentId - id of the GrantCallAttachment.
	 * @return A Object of GrantCallAttachment.
	 */
	public GrantCallAttachment fetchGrantCallAttachmentById(Integer attachmentId);

	/**
	 * This method is used to fetch GrantCall Contact Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of GrantCallContact.
	 */
	public List<GrantCallContact> fetchGrantCallContactBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdate GrantCallContact.
	 * @param grantCallContact - Object of the GrantCallContact.
	 * @return A Object of GrantCallContact.
	 */
	public GrantCallContact saveOrUpdateGrantCallContact(GrantCallContact grantCallContact);

	/**
	 * This method is used to delete GrantCall Contact.
	 * @param grantCallContact - Object of the GrantCallContact.
	 * @return A Object of GrantCallContact.
	 */
	public GrantCallContact deleteGrantCallContact(GrantCallContact grantCallContact);

	/**
	 * This method is used to delete GrantCall Contact.
	 * @param grantContactId - id of the GrantCallContact.
	 * @return A Object of GrantCallContact.
	 */
	public GrantCallContact fetchGrantCallContactById(Integer grantContactId);

	/**
	 * This method is used to fetch GrantCallResearchArea Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of GrantCallResearchArea.
	 */
	public List<GrantCallResearchArea> fetchGrantCallResearchAreaBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdateGrantCallResearchArea.
	 * @param grantCallResearchArea - Object of the GrantCallResearchArea.
	 * @return A Object of GrantCallResearchArea.
	 */
	public GrantCallResearchArea saveOrUpdateGrantCallResearchArea(GrantCallResearchArea grantCallResearchArea);

	/**
	 * This method is used to delete GrantCallResearchArea.
	 * @param grantCallResearchArea - Object of the GrantCallResearchArea.
	 * @return A Object of GrantCallResearchArea.
	 */
	public GrantCallResearchArea deleteGrantCallResearchArea(GrantCallResearchArea grantCallResearchArea);

	/**
	 * This method is used to delete GrantCallResearchArea.
	 * @param grantResearchAreaId - id of the GrantCallResearchArea.
	 * @return A Object of GrantCallResearchArea.
	 */
	public GrantCallResearchArea fetchGrantCallResearchAreaById(Integer grantResearchAreaId);

	/**
	 * This method is used to fetch GrantCallEligibility Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of GrantCallEligibility.
	 */
	public List<GrantCallEligibility> fetchGrantCallEligibilityBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdateGrantCallEligibility.
	 * @param grantCallEligibility - Object of the GrantCallEligibility.
	 * @return A Object of GrantCallEligibility.
	 */
	public GrantCallEligibility saveOrUpdateGrantCallEligibility(GrantCallEligibility grantCallEligibility);

	/**
	 * This method is used to delete GrantCallEligibility.
	 * @param grantCallEligibility - Object of the GrantCallEligibility.
	 * @return A Object of GrantCallEligibility.
	 */
	public GrantCallEligibility deleteGrantCallEligibility(GrantCallEligibility grantCallEligibility);

	/**
	 * This method is used to delete GrantCallEligibility.
	 * @param grantEligibilityId - id of the GrantCallEligibility.
	 * @return A Object of GrantCallEligibility.
	 */
	public GrantCallEligibility fetchGrantCallEligibilityById(Integer grantEligibilityId);

	/**
	 * This method is used to fetch GrantCallEligibleDepartment Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of GrantCallEligibleDepartment.
	 */
	public List<GrantCallEligibleDepartment> fetchGrantCallEligibleDepartmentBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdate GrantCallEligibleDepartment.
	 * @param grantCallEligibleDepartment - Object of the GrantCallEligibleDepartment.
	 * @return A Object of GrantCallEligibleDepartment.
	 */
	public GrantCallEligibleDepartment saveOrUpdateGrantCallEligibleDepartment(GrantCallEligibleDepartment grantCallEligibleDepartment);

	/**
	 * This method is used to delete GrantCallEligibleDepartment.
	 * @param grantCallEligibleDepartment - Object of the GrantCallEligibleDepartment.
	 * @return A Object of GrantCallEligibleDepartment.
	 */
	public GrantCallEligibleDepartment deleteGrantCallEligibleDepartment(GrantCallEligibleDepartment grantCallEligibleDepartment);

	/**
	 * This method is used to delete GrantCallEligibleDepartment.
	 * @param attachmentId - id of the GrantCallEligibleDepartment.
	 * @return A Object of GrantCallEligibleDepartment.
	 */
	public GrantCallEligibleDepartment fetchGrantCallEligibleDepartmentById(Integer attachmentId);

	/**
	 * This method is used to fetch GrantCallRelevant Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of GrantCallRelevant.
	 */
	public List<GrantCallRelevant> fetchGrantCallRelevantBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdate GrantCallRelevant.
	 * @param grantCallRelevant - Object of the GrantCallRelevant.
	 * @return A Object of GrantCallRelevant.
	 */
	public GrantCallRelevant saveOrUpdateGrantCallRelevant(GrantCallRelevant grantCallRelevant);

	/**
	 * This method is used to delete GrantCallRelevant.
	 * @param grantCallRelevant - Object of the GrantCallRelevant.
	 * @return A Object of GrantCallRelevant.
	 */
	public GrantCallRelevant deleteGrantCallRelevant(GrantCallRelevant grantCallRelevant);

	/**
	 * This method is used to delete GrantCallRelevant.
	 * @param grantCallRelevantId - id of the GrantCallRelevant.
	 * @return A Object of GrantCallRelevant.
	 */
	public GrantCallRelevant fetchGrantCallRelevantById(Integer attachmentId);

	/**
	 * This method is used to fetch ProposalEvaluationScore Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of ProposalEvaluationScore.
	 */
	public List<ProposalEvaluationScore> fetchProposalEvaluationScoreBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdate ProposalEvaluationScore.
	 * @param proposalEvaluationScore - Object of the ProposalEvaluationScore.
	 * @return A Object of ProposalEvaluationScore.
	 */
	public ProposalEvaluationScore saveOrUpdateProposalEvaluationScore(ProposalEvaluationScore proposalEvaluationScore);

	/**
	 * This method is used to delete ProposalEvaluationScore.
	 * @param proposalEvaluationScore - Object of the ProposalEvaluationScore.
	 * @return A Object of ProposalEvaluationScore.
	 */
	public ProposalEvaluationScore deleteProposalEvaluationScore(ProposalEvaluationScore proposalEvaluationScore);

	/**
	 * This method is used to fetch ProposalEvaluationScore Based Id.
	 * @param proposalEvalutionScoreId - id of the ProposalEvalutionScore.
	 * @return A Object of ProposalEvaluationScore.
	 */
	public ProposalEvaluationScore fetchProposalEvaluationScoreById(Integer proposalEvalutionScoreId);

	/**
	 * This method is used to fetch GrantCallIOIHeader Based On GrantCall Id.
	 * @param grantCallId - id of the GrantCall.
	 * @return A List of GrantCallIOIHeader.
	 */
	public List<GrantCallIOIHeader> fetchGrantCallIOIHeaderBasedOnGrantCallId(Integer grantCallId);

	/**
	 * This method is used to saveOrUpdate GrantCallIOIHeader.
	 * @param grantCallIOIHeader - Object of the GrantCallIOIHeader.
	 * @return A Object of GrantCallIOIHeader.
	 */
	public GrantCallIOIHeader saveOrUpdateGrantCallIOIHeader(GrantCallIOIHeader grantCallIOIHeader);

	/**
	 * This method is used to delete GrantCallIOIHeader.
	 * @param grantCallIOIHeader - Object of the GrantCallIOIHeader.
	 * @return A Object of GrantCallIOIHeader.
	 */
	public GrantCallIOIHeader deleteGrantCallIOIHeader(GrantCallIOIHeader grantCallIOIHeader);

	/**
	 * This method is used to fetch GrantCallIOIHeader Based on Id.
	 * @param grantCallIOIId - id of the GrantCallIOIHeader.
	 * @return A List of GrantCallIOIHeader.
	 */
	public GrantCallIOIHeader fetchGrantCallIOIHeaderById(Integer grantCallIOIId);

	/**
	 * This method is used to fetch GrantCallAttachment Based on GrantCall Id And DocumentId.
	 * @param grantCallIOIId - id of the GrantCall.
	 * @param documentId - documentId
	 * @return A List of GrantCallIOIHeader.
	 */
	public List<GrantCallAttachment> fetchGrantCallAttachmentBasedOnGrantCallIdAndDocumentId(Integer grantCallId, Integer documentId);

	/**
	 * This method is used to delete GrantCallActionLog list.
	 * @param grantCallActionLog - List of the GrantCallActionLog.
	 */
	public void deleteGrantCallActionLog(List<GrantCallActionLog> grantCallActionLog);

	/**
	 * This method is used to fetch GrantCallAttachment With Latest Version.
	 * @param grantCallIId - id of the GrantCall.
	 * @return A List of GrantCallAttachment.
	 */
	public List<GrantCallAttachment> fetchGrantCallAttachmentWithLastVersion(Integer grantCallId);

}
