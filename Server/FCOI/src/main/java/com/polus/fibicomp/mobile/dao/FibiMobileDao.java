package com.polus.fibicomp.mobile.dao;

import java.util.List;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.view.MobileProposalView;
import com.polus.fibicomp.vo.CommonVO;

@Service
public interface FibiMobileDao {

	/**
	 * @param personId - Person id.
	 * @param unitNumber - department number.
	 * @param isAdmin - flag for admin check.
	 * @param summaryTable for Research summary data for Mobile
	 * @return List of summary object
	 */
	public List<Object[]> getFibiSummaryTable(String personId, String unitNumber, boolean isAdmin, List<Object[]> summaryTable);

	/**
	 * This method is used to get list of proposal.
	 * @param vo - Object of CommonVO class.
	 * @return A list of proposal.
	 */
	public List<MobileProposalView> getProposalsByParams(CommonVO vo);

	/**
	 * This method is used to get list of proposals for certification.
	 * @param personId - Logged User ID
	 * @return a String of details of selected item
	 */
	public List<MobileProposalView> getProposalsForCertification(String personId);

	/**
	 * This method is used to get list of my proposals.
	 * @param vo - Object of CommonVO class.
	 * @return A list of proposals.
	 */
	public DashBoardProfile getMobileDashBoardDataForMyProposal(CommonVO vo);

	/**
	 * This method is used to get list of proposals for mobile application.
	 * @param proposalIds - A list of proposal id's.
	 * @return A list of proposals.
	 */
	public DashBoardProfile getDashBoardDataOfProposalsForMobile(List<Integer> proposalIds);
	
	/**
	 * This method is used to get list of approval in progress proposalIds.
	 * @param vo - Object of CommonVO class.
	 * @param proposalIds - A list of proposal id's.
	 * @return A list of proposals.
	 */
	public List<Integer> getApprovalInprogressProposalIdsForMobile(String personId, String approvalStatusCode, Integer moduleCode);

	/**
	 * This method is used to get list of proposalIds approved by other users and super users.
	 * @param personId - person id.
	 * @param moduleCode - moduleCode.
	 * @param proposalIds - A list of proposal id's.
	 * @return A list of proposalId's.
	 */
	public List<Integer> getFYIProposalIds(String personId, Integer moduleCode);

	/**
	 * This method is used to get list of proposalIds that have incomplete certification.
	 * @param personId - person id.
	 * @param proposalIds - A list of proposal id's.
	 * @return A list of proposalId's.
	 */
	public List<Integer> getCertificationInCompleteProposalIds(String personId);

	/**
	 * This method is used to get list of proposals that are uncertified.
	 * @param proposalIds - A list of proposal id's.
	 * @return A list of proposals.
	 */
	public DashBoardProfile getDashBoardDataOfUncertifiedProposals(List<Integer> proposalIds);

	/**
	 * This method is used to retrieve list of in_progress proposals.
	 * @param personId - ID of the user.
	 * @param isAdmin - flag that tells logged user is admin or PI.
	 * @param unitNumber - unit selected by the user.
	 * @return A list of Proposals in progress.
	 * @throws Exception 
	 */
	public DashBoardProfile getProposalsInProgressForMobile(String personId, boolean isAdmin, String unitNumber) throws Exception;

	/**
	 * This method is used to retrieve list of submitted proposals.
	 * @param personId - ID of the user.
	 * @param isAdmin - flag that tells logged user is admin or PI.
	 * @param unitNumber - unit selected by the user.
	 * @return A list of Submitted proposals.
	 * @throws Exception 
	 */
	public DashBoardProfile getSubmittedProposalsForMobile(String personId, boolean isAdmin, String unitNumber) throws Exception;
	
}
