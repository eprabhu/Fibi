package com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.dao;

import java.util.Set;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.ClaimFiles;
import com.polus.fibicomp.manpower.pojo.ManpowerConfigurationData;

@Transactional
@Service
public interface BaseSalaryDao {

	/**
	 * 
	 * @param sapConcurFiles
	 * @return
	 */
	public ClaimFiles saveManpowerSftpFiles(ClaimFiles baseSalaryclaimFiles);

	/**
	 * This method is used to get account code based on fileId.
	 * 
	 * @return set of account code
	 */
	public Set<String> getpersonIdByFileId(Integer fileId);

	/**
	 * This method is used to get the Manpower Configuration table value
	 * 
	 * @param parameterName
	 * @return
	 */
	public ManpowerConfigurationData getManpowerConfigurationValue(String parameterName);

}
