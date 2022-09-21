package com.polus.fibicomp.externaluser.service;

import javax.transaction.Transactional;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.externaluser.vo.HomeVo;

@Transactional
@Service
public interface ExternalUserService {

	/**
	 * Fetching user details with A P and D flag
	 * @param vo 
	 * @param vo 
	 * @return
	 */
	public String fetchExternalUserDetails(HomeVo vo);

	/**
	 * 
	 * @param vo
	 * @param userName 
	 * @return
	 */
	public String updateFlagByPersonId(HomeVo vo, String userName);

	/**
	 * Send External User Feed Details in CSV file to SFTP
	 */
	public void getExternalUserFeedDetails();

	/**
	 * Approved user build mail for External user
	 */
	public void externalUserApprovedMail();

	/**
	 * Fetch External User login details
	 * @param vo
	 * @return
	 */
	public String fetchExternalUserLoginDetails(HomeVo vo);

	public void sendRegisteredExternalUser();

}
