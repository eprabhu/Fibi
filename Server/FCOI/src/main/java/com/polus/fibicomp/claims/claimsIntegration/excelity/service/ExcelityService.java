package com.polus.fibicomp.claims.claimsIntegration.excelity.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import org.springframework.stereotype.Service;
import com.polus.fibicomp.claims.claimsIntegration.excelity.vo.ExcelityReportVo;

@Service
public interface ExcelityService {

	/**
	 * This method is used to do the File Processing for excelity
	 * 
	 */
	public void manpowerSftpForExcelity();

	/**
	 * This method is used to get Manpower SFTP Directory.
	 * 
	 * @param sftpWorkingDir - current Directory of the file.
	 * @param baseDirectory  - base directory of the file
	 */
	public void getExcelityResDirectory(String sftpWorkingDir, String baseDirectory);

	/**
	 * This method is used to move Manpower SFTP file.
	 * 
	 * @param file
	 * @param emailBody
	 * @param fileId 
	 */
	public void moveExcelityFile(File file, ExcelityReportVo excelityReportVo);

	/**
	 * This method is used to save Manpower SFTP file file.
	 * 
	 * @param file  - object of File.
	 * @param file2
	 * @return manpowerSftpReportVo - object of ManpowerSftpReportVo.
	 */
	public ExcelityReportVo saveExcelityFiles(File decrptdfile, String pgpFileName);

	/**
	 * 
	 * @param fileName
	 * @param fileReader
	 * @param vo
	 * @return
	 */
	public ExcelityReportVo excelityFileProcess(String fileName, BufferedReader fileReader, ExcelityReportVo vo);

	/**
	 * This method is used to get Manpower configuration from db as string.
	 * 
	 * @param parameterName - the value need to get from db.
	 * @return String - Y or not
	 */
	public String getSftpConfigurationValueAsString(String parameterName);

	/**
	 * This mehtod is used to decrypt the payroll file
	 * 
	 * @param file
	 * @return decrypted file
	 * @throws Exception
	 */
	public void pgpFileDecryption(File file, ExcelityReportVo excelityReportVo) throws Exception;

	/**
	 * 
	 * @param decrptdfile
	 */
	public void deleteDecryptedFile(File decrptdfile);

	/**
	 * 
	 * @param amount
	 * @return
	 * @throws Exception
	 */
	public String encryptAES(String data) throws Exception;
	
	/**
	 * 
	 * @param data
	 * @return
	 * @throws Exception
	 */
	public String decryptAESKey(String secretKey) throws Exception;
	
	/**
	 * 
	 * @param data
	 * @return
	 * @throws Exception
	 */
	public String decryptAESData(String data) throws Exception;
	
	/**
	 * 
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public Integer getRowCount(String filename) throws IOException;

}
