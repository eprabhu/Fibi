package com.polus.fibicomp.integratefile.service;

public interface IntegrationFileService {

	/**
	 * This method is used to read file data based on file path.
	 * @param filePath - file path
	 * @return .
	 * @throws Exception 
	 */
	public void readFileData(String filePath) throws Exception;

	/**
	 * This method is used to schedule time to integrate file.
	 * @return .
	 */
	public void scheduleIntegrationFile();

}
