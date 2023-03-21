package com.polus.fibicomp.mobile.service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service
public interface FibiMobileService {

	/**
	 * This method is to authenticate a mobile user
	 * @param loginMode - Login mode of the user.
	 * @param userName - userName of the user.
	 * @param password - Password of the user.
	 * @param request
	 * @param response
	 * @return JSON in mobile-profile format
	 * @throws Exception
	 */
	public String fibiMobileLogin(String login_mode, String userName, String password, HttpServletRequest request, HttpServletResponse response) throws Exception;

	/**
	 * This method is used to retrieve dashboard summary table data for FibiMobile.
	 * @param vo - CommonVO object.
	 * @return research summary table for fibiMobile
	 * @throws Exception
	 */
	public String getFibiResearchSummary(CommonVO vo) throws Exception;

	/**
	 * This method is used to get list of selected type in ResearchSummaryTable.
	 * @param vo - CommmonVO object.
	 * @return a String of details of selected item
	 * @throws Exception
	 */
	public String getFibiResearchSummaryData(CommonVO vo) throws Exception;

	/**
	 * This method is used to get list of proposals for certification.
	 * @param personId - Logged User ID
	 * @return a String of details of selected item
	 */
	public String getProposalsForCertification(String personId);

	/**
	 * This method is used to get list of proposals.
	 * @param vo - object of CommonVO.
	 * @returnS proposal list.
	 * @throws Exception
	 */
	public String getProposals(CommonVO vo) throws Exception;

	/**
	 * This method is used to approve or disapprove a proposal.
	 * @param formDataJSON - Request object data.
	 * @return a String of details of proposal.
	 */
	public String approveOrRejectProposalForMobile(String formDataJSON);

	/**
	 * This method is used to load proposal based on id.
	 * @param proposalId - Id of the proposal.
	 * @param personId - currentUser.
	 * @return A string of details of a proposal.
	 */
	public String loadProposalByIdForMobile(Integer proposalId, String personId);

}
