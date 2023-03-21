package com.polus.fibicomp.ip.dao;

import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionLog;
import com.polus.fibicomp.ip.pojo.InstituteProposalActionType;
import com.polus.fibicomp.ip.pojo.InstituteProposalAdminDetail;
import com.polus.fibicomp.ip.pojo.InstituteProposalAttachType;
import com.polus.fibicomp.ip.pojo.InstituteProposalAttachments;
import com.polus.fibicomp.ip.pojo.InstituteProposalBudgetHeader;
import com.polus.fibicomp.ip.pojo.InstituteProposalComment;
import com.polus.fibicomp.ip.pojo.InstituteProposalKeywords;
import com.polus.fibicomp.ip.pojo.InstituteProposalPerson;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonUnit;
import com.polus.fibicomp.ip.pojo.InstituteProposalResearchArea;
import com.polus.fibicomp.ip.pojo.InstituteProposalSpecialReview;
import com.polus.fibicomp.ip.pojo.InstituteProposalStatus;
import com.polus.fibicomp.ip.pojo.InstituteProposalType;
import com.polus.fibicomp.ip.pojo.InstituteProposalVersionHistory;
import com.polus.fibicomp.ip.vo.InstProposalVO;
import com.polus.fibicomp.pojo.SpecialReviewType;
import com.polus.fibicomp.proposal.pojo.ProposalAttachment;

@Service
public interface InstitutionalProposalDao {

	/**
	 * This method is used to create institutional proposal.
	 * @param proposalId - Id of the proposal.
	 * @param ipNumber   - Institutional proposal number.
	 * @param userName   - Username of the logged in user.
	 * @return a boolean value.
	 */
	public boolean createInstitutionalProposal(Integer proposalId, String ipNumber, String userName);

	/**
	 * This method is used to fetch institutional proposal by id.
	 * @param proposalId - Id of the institute proposal.
	 * @return corresponding InstituteProposal object.
	 */
	public InstituteProposal fetchInstProposalById(Integer proposalId);

	/**
	 * This method is used to fetch institutional proposal attachments by attachment
	 * id.
	 * @param attachmentId - Id of the institute proposal attachments.
	 * @return corresponding InstituteProposalAttachment object.
	 */
	public InstituteProposalAttachments fetchAttachmentById(Integer attachmentId);

	/**
	 * This method is used to fetch development proposal by institute proposal.
	 * @param proposalId - Id of the institute proposal.
	 * @return corresponding development proposal id of institute proposal.
	 */
	public List<Integer> fetchDevProposalByInstProposal(Integer proposalId);

	/**
	 * This method is used to fetch Institute Proposal Status By Id.
	 * @param statusCode - statusCode of the institute proposal.
	 * @return corresponding InstituteProposalStatus.
	 */
	public InstituteProposalStatus fetchInstituteProposalStatusById(Integer statusCode);

	/**
	 * This method is used to saveOrUpdateInstituteProposal.
	 * @param instituteProposal - object of the institute proposal.
	 * @return instituteProposal.
	 * @throws Exception 
	 */
	public InstituteProposal saveOrUpdateInstituteProposal(InstituteProposal instituteProposal);

	/**
	 * This method is used to fetchAllInstituteProposalStatus.
	 * @return List of InstituteProposalStatus.
	 */
	public List<InstituteProposalStatus> fetchAllInstituteProposalStatus();

	/**
	 * This method is used to fetch sorted institute proposal attachment list. 
	 * @param proposalId  - proposal id.
	 * @param sortBy - sort by field name.
	 * @param reverse - reverse type asc or desc
	 * @return list of institute proposal attachments.
	 */
	public List<InstituteProposalAttachments> getInstituteProposalAttachments(Integer proposalId);

	/**
	 * This method is used to select the proposal persons by proposal Id
	 * @param proposalId
	 * @return list of all proposal persons
	 */
	public List<InstituteProposalPerson> loadInstProposalPersonsByProposalId(Integer proposalId);

	/**
	 * This method is used to select all the special review of IP by proposal id
	 * @param proposalId
	 * @return list all proposal special review
	 */
	public List<InstituteProposalSpecialReview> loadInstProposalSpecialReviewByProposalId(Integer proposalId);

	/**
	 * This method is used to select all the research area of institute proposal
	 * @param proposalId
	 * @return list of all research areas
	 */
	public List<InstituteProposalResearchArea> loadInstProposalResearchAreaByProposalId(Integer proposalId);

	/**
	 * This method is used to load the budget details by proposal id
	 * @param proposalId
	 * @return list of all budgets
	 */
	public InstituteProposalBudgetHeader loadInstPropBudgetDetailsByProposalId(Integer proposalId);

	/**
	 * This method is used to get all the dev proposal attachmnet by devproposal id
	 * @param devProposalId
	 * @return list of proposal attachments
	 */
	public List<ProposalAttachment> fetchProposalAttachmentsByProposalIds(Integer devProposalId);

	/**
	 * This method is used to save IP person details
	 * @param copyInstituteProposalPerson
	 */
	public void saveOrUpdateInstituteProposalPerson(InstituteProposalPerson copyInstituteProposalPerson);

	/**
	 * This method is used to save the IP research area
	 * @param copyInstituteProposalResearchArea
	 */
	public void saveOrUpdateIPResearchAreas(InstituteProposalResearchArea copyInstituteProposalResearchArea);

	/**
	 * This method is used to save the IP budget header
	 * @param copyInstituteProposalBudgetHeader
	 */
	public void saveOrUpdateIPBudgetDetails(InstituteProposalBudgetHeader copyInstituteProposalBudgetHeader);

	/**
	 * This method is used to save IP special review
	 * @param copyIPSpecialReview
	 */
	public void saveOrUpdateSpecialReview(InstituteProposalSpecialReview copyIPSpecialReview);

	public List<InstituteProposalAdminDetail> loadAllProposalAdminDetails(Integer proposalId);

	public void saveOrUpdateIPAdminDetail(InstituteProposalAdminDetail copyIPAdminDetail);

	public void changeIPStatus(Integer statusCode, Integer proposalId);

	public void deleteInstProposalPersonUnit(InstituteProposalPersonUnit proposalPersonUnit);

	public void deleteIPKeyPerson(Integer keyPersonId);

	public void deleteIPPersonAttachment(Integer replaceAttachmentId);

	public void saveOrUpdateIPSpecialReview(InstituteProposalSpecialReview instituteProposalSpecialReview);

	public void saveOrUpdateIPAreaOfResearch(InstituteProposalResearchArea instituteProposalResearchArea);

	public void saveOrUpdateIPBudget(InstituteProposalBudgetHeader instituteProposalBudgetHeader);

	public void deleteIPSpecialReview(Integer specialReviewId);

	public void deleteIPAreaOfResearch(Integer areaOfResearchId);

	public void deleteIPBudgetData(Integer budgetHeaderId);

	public void saveOrUpdateIPAttachment(InstituteProposalAttachments instituteProposalAttachment);

	public void deleteIPAttachment(Integer attachmentId);

	public InstituteProposalAttachments fetchIPAttachmentById(Integer attachmentId);

	public void updateSequenceStatus(Integer proposalId, String string);

	public void saveIPVersionHistory(InstituteProposalVersionHistory history);

	public void saveOrUpdateActionLog(InstituteProposalActionLog actionLog);

	public void saveOrUpdateIPKeyword(InstituteProposalKeywords instituteProposalKeyword);

	public List<InstituteProposalKeywords> fetchAllIPKeywordByProposal(Integer proposalId);

	public void deleteIPKeyword(Integer keywordId);

	public void updateProposalTimestampAndUser(Integer proposalId);

	public List<SpecialReviewType> getSpecialReviewTypes(Integer moduleCode);

	public Integer getNextSequenceNumber(String proposalNumber);

	public List<InstituteProposalAttachType> fetchAllIPAttachmentTypes();

	public List<InstituteProposalType> fetchAllIPTypes();

	public InstituteProposalVersionHistory getIPVersionHistory(Integer originatedProposal);

	public List<InstituteProposalActionType> fetchIPActionTypes();

	public List<InstituteProposalActionLog> getIPActionHistory(Integer proposalId);

	public void mergeProposalToIP(Integer proposalId, InstProposalVO vo) throws SQLException;

	public Boolean anyAwardLinkedToIP(String proposalNumber);

	public Boolean anyAwardInPendingSequence(String proposalNumber);

	Integer getActiveInstProposalId(String proposalNumber);

	public Boolean checkForPIDifference(String proposalNumber, Integer devProposalId);

	public String getIPTitleForMasterProposal(String baseProposalNumber);

	/**
	 * This method is used to fetch institute proposal comments.
	 * @param proposalId
	 * @return it returns proposalComment list based on proposal id , user name
	 */
	public List<InstituteProposalComment> fetchInstituteProposalCommentsByParams(Integer proposalId);

	/**
	 * This method is used to save or update institute proposal comment.
	 * @param instituteProposalComment
	 * @return InstituteProposalComment
	 */
	public InstituteProposalComment saveOrUpdateInstituteProposalComment(InstituteProposalComment instituteProposalComment);

	/**
	 * This method is used to get Title of instituteProposal.
	 * @param instituteProposalNumber
	 * @return Title
	 */
	public String getIPTitle(String baseProposalNumber);

	/**
	 * This method is used to get update budget start and date  of instituteProposal.
	 * @param startDate
	 * @param endDate
	 * @param proposalId
	 */
	public void updateIPBudgetDates(Timestamp startDate, Timestamp endDate, Integer proposalId);

	/**
	 * This method is used to get history details of instituteProposal.
	 * @param proposalNumber
	 */
	public List<Object[]> fetchIPHistoryDetails(String proposalNumber);

	/**
	 * Get IP For Master Proposal
	 *
	 * @param baseProposalNumber
	 * @return
	 */
	InstituteProposal getIPForMasterProposal(String baseProposalNumber);

}
