package com.polus.fibicomp.proposal.module.dao;

import java.util.List;

import com.polus.fibicomp.proposal.pojo.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.evaluation.pojo.ProposalReview;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.prereview.pojo.PreReview;

@Transactional
@Service
public interface ProposalModuleDao {

	/**
	 * This method is used to fetchProposal AttachmentBased On Proposal Id.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalAttachment.
	 */
	public List<ProposalAttachment> fetchProposalAttachmentBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch BudgetHeader Based On ProposalId.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of BudgetHeader.
	 */
	public List<BudgetHeader> fetchBudgetHeaderBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetchProposal Keyword Based On Proposal Id.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalKeyword.
	 */
	public List<ProposalKeyword> fetchProposalKeywordBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalPerson Based On ProposalId.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalPerson.
	 */
	public List<ProposalPerson> fetchProposalPersonBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalIrbProtocol Based On ProposalId.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalIrbProtocol.
	 */
	public List<ProposalIrbProtocol> fetchProposalIrbProtocolBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalResearchArea Based On Proposal Id.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalResearchArea.
	 */
	public List<ProposalResearchArea> fetchProposalResearchAreaBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalSponsor Based On ProposalId.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalSponsor.
	 */
	public List<ProposalSponsor> fetchProposalSponsorBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalProjectTeam Based On ProposalId.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalProjectTeam.
	 */
	public List<ProposalProjectTeam> fetchProposalProjectTeamBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalReview Based On Proposal Id.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalReview.
	 */
	public List<ProposalReview> fetchProposalReviewBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalPersonAssignedRoles Based On ProposalId.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalPersonAssignedRoles.
	 */
	public List<ProposalPersonAssignedRoles> fetchProposalPersonAssignedRolesBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to fetch ProposalSpecialReview Based On ProposalId.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalSpecialReview.
	 */
	public List<ProposalSpecialReview> fetchProposalSpecialReviewBasedOnProposalId(Integer proposalId);

	public ProposalAttachment saveOrUpdateProposalAttachment(ProposalAttachment proposalAttachment);

	public ProposalAttachment deleteProposalAttachment(ProposalAttachment proposalAttachment);

	public ProposalAttachment fetchProposalAttachment(Integer attachmentId);

	public ProposalResearchArea saveOrUpdateProposalResearchArea(ProposalResearchArea proposalResearchArea);

	public ProposalResearchArea deleteProposalResearchArea(ProposalResearchArea proposalResearchArea);

	public ProposalResearchArea fetchProposalResearchArea(Integer researchAreaId);

	public ProposalPerson saveOrUpdateProposalPerson(ProposalPerson proposalPerson);

	public ProposalSponsor fetchProposalSponsor(Integer sponsorId);

	public ProposalSponsor deleteProposalSponsor(ProposalSponsor proposalSponsor);

	public ProposalSponsor saveOrUpdateProposalSponsor(ProposalSponsor proposalSponsor);

	public ProposalPerson fetchProposalPerson(Integer proposalPersonId);

	public ProposalPerson deleteProposalPerson(ProposalPerson proposalPerson);

	public ProposalIrbProtocol saveOrUpdateProposalIrbProtocol(ProposalIrbProtocol proposalIrbProtocol);

	public ProposalIrbProtocol deleteProposalIrbProtocol(ProposalIrbProtocol proposalIrbProtocol);

	public ProposalIrbProtocol fetchProposalIrbProtocol(Integer irbProtocolId);

	public ProposalSpecialReview saveOrUpdateProposalSpecialReview(ProposalSpecialReview proposalSpecialReview);

	public ProposalSpecialReview deleteProposalSpecialReview(ProposalSpecialReview proposalSpecialReview);

	public ProposalSpecialReview fetchProposalSpecialReview(Integer id);

	public ProposalReview saveOrUpdateProposalReview(ProposalReview proposalReview);

	public ProposalReview deleteProposalReview(ProposalReview proposalReview);

	public ProposalReview fetchProposalReview(Integer reviewId);

	public ProposalPersonAssignedRoles saveOrUpdateProposalPersonAssignedRoles(ProposalPersonAssignedRoles proposalPersonAssignedRoles);

	public ProposalPersonAssignedRoles deleteProposalPersonAssignedRoles(ProposalPersonAssignedRoles proposalPersonAssignedRoles);

	public ProposalPersonAssignedRoles fetchProposalPersonAssignedRoles(Integer personRoleId);

	public ProposalProjectTeam saveOrUpdateProposalProjectTeam(ProposalProjectTeam proposalProjectTeam);

	public BudgetHeader deleteBudgetHeader(BudgetHeader budgetHeader);

	public List<ProposalAttachment> fetchProposalAttachmentBasedOnAttachmentIds(List<Integer> attachmentIds);

	public ProposalProjectTeam deleteProposalProjectTeam(ProposalProjectTeam proposalProjectTeam);

	public List<ProposalAttachment> fetchProposalAttachmentBasedOnProposalIdAndDocumentId(Integer proposalId, Integer documentId);

	public ProposalPersonUnit deleteProposalPersonUnit(ProposalPersonUnit proposalPersonUnit);

	/**
	 * This method is used to fetch ProposalMileStone Based On proposalMileStoneId.
	 * @param proposalMileStoneId - proposalMileStoneId of the proposalMileStone.
	 * @return An object of ProposalMileStone.
	 */
	public ProposalMileStone fetchProposalMileStone(Integer proposalMileStoneId);

	/**
	 * This method is used to save ProposalMileStone.
	 * @param proposalMileStone - ProposalMileStone object.
	 * @return An object of ProposalMileStone.
	 */
	public ProposalMileStone saveOrUpdateProposalMileStone(ProposalMileStone proposalMileStone);

	/**
	 * This method is used to fetch Proposal MileStone Based On Proposal Id.
	 * @param proposalId - proposalId of the proposal.
	 * @return An object of ProposalMileStone.
	 */
	public List<ProposalMileStone> fetchProposalMileStonesBasedOnProposalId(Integer proposalId);

	/**
	 * This method is used to delete Proposal MileStone.
	 * @param proposalMileStone - ProposalMileStone object.
	 * @return An object of ProposalMileStone.
	 */
	public ProposalMileStone deleteProposalMileStone(ProposalMileStone proposalMileStone);

	public ProposalKPI deleteProposalKPI(ProposalKPI proposalKPI);

	public List<ProposalKPI> fetchProposalKPIBasedOnProposalId(Integer proposalId);

	public ActivityType fetchActivityTypeBasedInId(String activityTypeCode);

	public List<ProposalPersonRoles> fetchProposalPersonRolesBasedOnProposalId(Integer proposalId);

	public ProposalPersonRoles deleteProposalPersonRoles(ProposalPersonRoles proposalPersonRoles);

	public List<PreReview> fetchPreReviewBasedOnProposalId(Integer proposalId);

	public PreReview deletePreReview(PreReview preReview);

	public String getPrincipalInvestigator(Integer proposalId);

	public ProposalPersonUnit saveOrUpdateProposalPersonUnit(ProposalPersonUnit proposalPersonUnit);

	/**
	 * This method is used to fetch ProposalFundingStatus Based On fundingStatusCode.
	 * @param fundingStatusCode - fundingStatusCode.
	 * @return An object of ProposalFundingStatus.
	 */
	public ProposalFundingStatus fetchProposalFundingStatusById(Integer fundingStatusCode);

	/**
	 * This method is used to fetch Sponsor Type Based On Id.
	 * @param sponsorTypeCode - sponsorTypeCode.
	 * @return An object of SponsorType.
	 */
	public SponsorType fetchSponsorTypeById(String sponsorTypeCode);

	/**
	 * This method is used to get the principle investigator name
	 * @param proposalId
	 * @return principle investigator name
	 */
	public String getPrincipalInvestigatorName(Integer proposalId);

	/**
	 * This method is used to save Proposal Person Attachment
	 *
	 * @param personAttachment
	 * @return
	 */
	ProposalPersonAttachment saveOrUpdateProposalPersonAttachment(ProposalPersonAttachment personAttachment);

	/**
	 * This method returns ProposalPersonAttachment by its id
	 *
	 * @param attachmentId
	 * @return
	 */
	ProposalPersonAttachment getProposalPersonAttachmentById(Integer attachmentId);
	
	/**
	 * This method update key personal attachment details
	 * @param attachmentId
	 * @param description
	 * @param attachmentTypeCode
	 */
	public void updateProposalkeyPersonAttachment(Integer attachmentId, String description, Integer attachmentTypeCode );

	 /**
	 * This method is used to fetchProposal AttachmentBased On Proposal ID with the Latest Version
	 * @param proposalId Proposal ID
	 * @return List of ProposalAttachment
	 */
	 List<ProposalAttachment> fetchProposalAttachmentByProposalIdWithLastVersion(Integer proposalId);

   /**
	 * This method is used to add degree against a person in proposal.
	 * @param attachmentId
	 */
	public void addProposalPersonDegree(ProposalPersonDegree proposalPersonDegree);

	 /**
	 * This method  is used to get all degree of a person in a proposal.
	 * @param proposalPersonId
	 * @return list of person degree.
	 */
	public List<ProposalPersonDegree> getPersonDegree(Integer proposalPersonId);

	 /**
	 * This method is used to delete a degree of a person in a proposal.
	 * @param proposalPersonDegreeId
	 * @return
	 */
	public void deleteProposalPersonDegree(Integer proposalPersonDegreeId); 
	
	 /**
	 * This method is used to delete a degree of a person in a proposal using Proposal person id.
	 * @param proposalPersonId
	 * @return
	 */
	public void deleteDegreeByProposalPersonId(Integer proposalPersonId); 

}
