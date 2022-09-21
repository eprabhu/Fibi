package com.polus.fibicomp.claims.claimsIntegration.sapconcur.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.ClaimFiles;
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.pojo.ConcurStaffTravelDtls;

@Transactional
@Service
public interface SapConcurDao {

	/**
	 * 
	 * @param concurStaffTravelDtls
	 * @return
	 */
	public void saveSapConcurSftp(ConcurStaffTravelDtls concurStaffTravelDtls);

	/**
	 * 
	 * @param sapConcurFiles
	 * @return
	 */
	public ClaimFiles saveSapConcurSftpFiles(ClaimFiles concurClaimFiles);

	/**
	 * 
	 * @param fileId
	 * @return
	 */
	public List<String> getaccountCodeByFileId(Integer fileId);

}
