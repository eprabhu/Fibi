package com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.service;

import java.io.BufferedReader;
import java.io.File;
import org.springframework.stereotype.Service;
import com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.vo.BaseSalaryVo;

@Service
public interface BaseSalaryService {

	/**
	 * This method is used to do the File Processing for Base Salary
	 * 
	 */
	public void manpowerBaseSalary();

	/**
	 * This method is used to get Manpower SFTP Directory.
	 * 
	 * @param sftpWorkingDir - current Directory of the file.
	 * @param baseDirectory  - base directory of the file
	 */
	public void getBaseSalaryResDirectory(String sftpWorkingDir, String baseDirectory);

	/**
	 * This method is used to move Manpower SFTP file.
	 * 
	 * @param file
	 * @param emailBody
	 * @param fileId
	 */
	public void moveBaseSalaryFile(File file, StringBuilder emailBody, Integer fileId);

	/**
	 * This method is used to save Manpower SFTP file file.
	 * 
	 * @param file  - object of File.
	 * @param file2
	 * @return manpowerSftpReportVo - object of ManpowerSftpReportVo.
	 */
	public BaseSalaryVo saveBaseSalaryFiles(File decrptdfile, String pgpFileName);

	/**
	 * This method is used to process the manpower Base Salary file
	 * 
	 * @param fileName
	 * @param fileReader
	 * @param file
	 * @param vo
	 * @return ManpowerSftpReportVo
	 */
	public BaseSalaryVo baseSalaryFileProcess(String fileName, BufferedReader fileReader, BaseSalaryVo vo);

	/**
	 * This method is used to get Manpower configuration from db as string.
	 * 
	 * @param parameterName - the value need to get from db.
	 * @return String - Y or not
	 */
	public String getSftpConfigurationValueAsString(String parameterName);

	/**
	 * 
	 * @param file
	 * @param baseSalaryVo
	 * @throws Exception
	 */
	public void pgpFileDecryption(File file, BaseSalaryVo baseSalaryVo) throws Exception;

	/**
	 * 
	 * @param decrptdfile
	 */
	public void deleteDecryptedFile(File decrptdfile);

	
}
