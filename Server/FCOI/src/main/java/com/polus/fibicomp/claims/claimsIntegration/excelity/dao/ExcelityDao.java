package com.polus.fibicomp.claims.claimsIntegration.excelity.dao;

import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.ClaimFiles;
import com.polus.fibicomp.manpower.pojo.AwardManpowerPayroll;
import com.polus.fibicomp.manpower.pojo.ManpowerConfigurationData;

@Transactional
@Service
public interface ExcelityDao {

	/**
	 * This method is used to save Manpower SFTP data
	 * 
	 * @param awardManpowerPayroll
	 * @return list of awardManpower resources
	 */
	public AwardManpowerPayroll saveManpowerSftp(AwardManpowerPayroll awardManpowerPayroll);

	/**
	 * This method is used to save Manpower SFTP payroll file.
	 * 
	 * @param awardManpowerPayrollFile - object of AwardManpowerPayrollFile.
	 * @return awardManpowerPayrollFile -object of AwardManpowerPayrollFile.
	 */	
	public ClaimFiles saveManpowerSftpFiles(ClaimFiles awardManpowerPayrollFile);

	/**
	 * This method is used to get account code based on fileId.
	 * 
	 * @return set of account code
	 */
	public Set<String> getaccountCodeByFileId(Integer fileId);

	/**
	 * This method is used to get the Manpower Configuration table value
	 * 
	 * @param parameterName
	 * @return
	 */
	public ManpowerConfigurationData getManpowerConfigurationValue(String parameterName);

}
