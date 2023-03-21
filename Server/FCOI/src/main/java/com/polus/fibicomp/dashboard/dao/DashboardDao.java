package com.polus.fibicomp.dashboard.dao;

import java.util.List;

import javax.validation.Valid;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.dashboard.vo.AgreementDashboardVO;
import com.polus.fibicomp.dashboard.vo.AwardDashboardVO;
import com.polus.fibicomp.dashboard.vo.ClaimDashboardVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.dashboard.vo.GrantCallDashboardVO;
import com.polus.fibicomp.dashboard.vo.InstituteProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.NegotiationDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProgressReportDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.ServiceRequestDashboardVO;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.FileType;
import com.polus.fibicomp.vo.CommonVO;

/**
 * @author sasi
 *
 */
@Service
public interface DashboardDao {

	/**
	 * This method is used to get list of COI Disclosure.
	 * @param vo - Object of CommonVO class.
	 * @return A list of disclosure.
	 */
	public DashBoardProfile getDashBoardDataForDisclosures(CommonVO vo);

	/**
	 * This method is used to get list of Committee.
	 * @param vo - Object of CommonVO class.
	 * @return A list of committee.
	 */
	public DashBoardProfile getDashBoardDataForCommittee(CommonVO vo);

	/**
	 * This method is used to get list of CommitteeSchedule.
	 * @param vo - Object of CommonVO class.
	 * @return A list of committe schedule.
	 */
	public DashBoardProfile getDashBoardDataForCommitteeSchedule(CommonVO vo);

	/**
	 * This method is used to get list of Grant Calls.
	 * @param vo - Object of GrantCallDashboardVO class.
	 * @return A list of grantCalls.
	 */
	public DashBoardProfile getDashBoardDataForGrantCall(GrantCallDashboardVO vo);

	/**
	 * This method is used to get list of awards.
	 * @param vo - Object of CommonVO class.
	 * @param awards - for award data.
	 * @return A list of awards.
	 * @throws Exception
	 */
	public List<Object[]> getDashBoardDataForAwardForDownload(String personId, String sponsorCode,List<Object[]> awards) throws Exception;

	/**
	 * This method is used to get list of protocols.
	 * @param vo - Object of CommonVO class.
	 * @param protocols - for protocol data.
	 * @return A list of protocols.
	 * @throws Exception
	 */
	public List<Object[]> getProtocolDashboardDataForDownload(String personId, String sponsorCode,List<Object[]> protocols) throws Exception;
	
	/**
	 * This method is used to get list of Negotiations.
	 * @param vo - Object of CommonVO class.
	 * @return A list of negotiations.
	 * @throws Exception
	 */
	public DashBoardProfile getDashBoardDataForNegotiations(NegotiationDashboardVO vo);

	/**
	 * This method is used to get list of institute proposals.
	 * @param vo - Object of InstituteProposalDashboardVO class.
	 * @return A list of Institute proposals.
	 */	
	public DashBoardProfile getDashBoardDataForInstProposal(InstituteProposalDashboardVO vo);

	/**
	 * This method is used to fetch list of agreements.
	 * @param  vo- Object of AgreementDashboardVO class.
	 * @return A list of agreements.
	 */
	public DashBoardProfile getDashBoardDataForAgreement(AgreementDashboardVO vo);

	public List<Object[]> getDashBoardDataOfPersonForDownload(CommonVO vo, List<Object[]> personData);

	public List<Object[]> getDataOfRolodexForDownload(CommonVO vo, List<Object[]> dashboardData);

	/**
	 * This method is used to fetch the dash-board data based on proposal tabs.
	 * @param vo
	 * @return object of DashBoardProfile
	 */
	public DashBoardProfile dashboardDatasForProposal(ProposalDashboardVO vo);

//	/**
//	 * This method is used to get Pending Award Numbers.
//	 * @param personId - Logged in person Id.
//	 * @return A set of Award Numbers.
//	 */
//	public Set<Integer> getPendingAwardNumbers(String personId);
//
//	/**
//	 * This method is used to get list of awards.
//	 * @param vo- Object of CommonVO class.
//	 * @return A list of active awards.
//	 */
//	public DashBoardProfile getDashBoardDataForPendingAward(CommonVO vo, Set<Integer> pendingAwards);
//
//	/**
//	 * This method is used to get list of active awards.
//	 * @param vo - Object of CommonVO class.
//	 * @param awards - for award data.
//	 * @return A list of awards.
//	 * @throws Exception
//	 */
//	public List<Award> getDashBoardDataOfMyAwardForDownload(CommonVO vo, Set<Integer> activeAwards);
//
//	/**
//	 * This method is used to get list of pending awards.
//	 * @param vo - Object of CommonVO class.
//	 * @param awards - for award data.
//	 * @return A list of awards.
//	 * @throws Exception
//	 */
//	public List<Award> getDashBoardDataOfPendingAwardForDownload(CommonVO vo, Set<Integer> pendingAwards);

//	/**
//	 * This method is used to get Pending Award Numbers.
//	 * @param personId - Logged in person Id.
//	 * @return A set of Award Numbers.
//	 */
//	public Set<Integer> getDraftAwardNumbers(String personId);
//
//	/**
//	 * This method is used to get list of draft awards .
//	 * @param vo- Object of CommonVO class.
//	 * @return A list of active awards.
//	 */
//	public DashBoardProfile getDashBoardDataForDraftAward(CommonVO vo, Set<Integer> awardNumbers);
//
//	/**
//	 * This method is used to get list of pending awards.
//	 * @param vo - Object of CommonVO class.
//	 * @param awards - for award data.
//	 * @return A list of awards.
//	 * @throws Exception
//	 */
//	public List<Award> getDashBoardDataOfDraftAwardForDownload(CommonVO vo, Set<Integer> draftAwards);
//
//	/**
//	 * This method is used to get all Award Numbers.
//	 * @param personId - Logged in person Id.
//	 * @return A set of Award Numbers.
//	 */
//	public Set<Integer> getAllAwardNumbers();
//
//	/**
//	 * This method is used to get list of All awards .
//	 * @param vo- Object of CommonVO class.
//	 * @return A list of all awards.
//	 */
//	public DashBoardProfile getDashBoardDataForAllAward(CommonVO vo, Set<Integer> allAwards);
//
//	/**
//	 * This method is used to get list of pending awards.
//	 * @param vo - Object of CommonVO class.
//	 * @param awards - for award data.
//	 * @return A list of awards.
//	 * @throws Exception
//	 */
//	public List<Award> getDashBoardDataOfAllAwardForDownload(CommonVO vo, Set<Integer> allAwards);

	/**
	 * This method is used to get dash-board datas for award based on the tab 
	 * @param vo
	 * @return list of awards
	 */
	public DashBoardProfile dashboardDatasForAward(AwardDashboardVO vo);

	/**
	 * This method is used to get all service request datas for dashboard
	 * @param vo
	 * @return list of service requests
	 */
	public DashBoardProfile loadServiceRequestDashBoard(ServiceRequestDashboardVO vo);

	/**
	 * This method is used to get all file types
	 * 
	 * @return list of file types
	 */
	public List<FileType> getAllFileTypes();

	/**
	 * This method is used to get claim dashboard data
	 * @param vo
	 * @return List of claims
	 */
	public DashBoardProfile getClaimDashBoardData(ClaimDashboardVO vo);

	/**
	 * @param vo
	 * @return list of award progress report
	 */
	public DashBoardProfile fibiProgressReportDashBoard(ProgressReportDashboardVO vo);

	/**
	 * This method is used to list agreements based on category.
	 * @param vo
	 * @return
	 */
	public DashBoardProfile getDashBoardDataForAgreementBasedOnCategory(AgreementDashboardVO vo);

	/**
	 * @param vo
	 * @return list of coi
	 */
	public DashBoardProfile getCOIDashboard(CoiDashboardVO vo);

	/**
	 * This method is used to get list of coi for admin
	 * @param vo
	 * @return list of coi
	 */
	public DashBoardProfile getCOIAdminDashboard(CoiDashboardVO vo);

	/**
	 * This method is used to get list of sfi
	 * @param vo
	 * @return list of sfi
	 */
	public DashBoardProfile getSFIDashboard(CoiDashboardVO vo);

}
