package com.polus.fibicomp.claims.claimsIntegration.sapconcur.service;

import java.io.BufferedReader;
import java.io.File;
import org.springframework.stereotype.Service;

import com.jcraft.jsch.SftpException;
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.vo.SapConcurVo;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;

@Service
public interface SapConcurService {

	/**
	 * This method is used to do the File Processing for sapConcur
	 * 
	 */
	public void sapConcurSftp();

	/**
	 * This method is used to get Manpower SFTP Directory.
	 * 
	 * @param sftpWorkingDir - current Directory of the file.
	 * @param baseDirectory  - base directory of the file
	 * @param emailContent 
	 * @throws SftpException 
	 */
	public void getSapConcurResDirectory(String sftpWorkingDir, String baseDirectory, EmailContent emailContent);

	/**
	 * This method is used to move Manpower SFTP file.
	 * 
	 * @param file
	 * @param emailBody
	 * @param fileId
	 */
	public void moveSapConcurFile(File file, StringBuilder emailBody, Integer fileId);

	/**
	 * 
	 * @param file
	 * @return
	 */
	public SapConcurVo saveSapConcurFiles(File file, SapConcurVo sapConcurVo);

	/**
	 * This method is used to process the manpower excelity file
	 * 
	 * @param fileName
	 * @param fileReader
	 * @param file
	 * @param vo
	 * @return ManpowerSftpReportVo
	 */
	public SapConcurVo sapConcurFileProcess(String fileName, BufferedReader fileReader, SapConcurVo vo);

	/**
	 * This method is used to get Manpower configuration from db as string.
	 * 
	 * @param parameterName - the value need to get from db.
	 * @return String - Y or not
	 */
	public String getSftpConfigurationValueAsString(String parameterName);

}
