package com.polus.fibicomp.claims.claimsIntegration.excelity.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.fibicomp.claims.claimsIntegration.excelity.pojo.SftpConfigurationData;

@Transactional
@Service
public interface SftpConfigurationService {

	/**
	 * 
	 * @param parameterName
	 * @return
	 */
	public SftpConfigurationData getSFTPConfigurationValue(String parameterName);

	/**
	 * 
	 * @param parameterName
	 * @return
	 */
	public String getSftpConfigurationValueAsString(String parameterName);

}
