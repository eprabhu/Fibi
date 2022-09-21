package com.polus.fibicomp.roles.service;

import java.util.ArrayList;

public interface AuthorizationService {
	
	/** List all system level right of a person
	 * String: personId* 
	 * @return List of RIGHT_NAME
	 */
	public String allSystemLevelPermission(String personId);
	
	/**
	 * List all department level right of a person
	 * @param personId
	 * @param moduleCode
	 * @param leadUnit
	 * @return List of RIGHT_NAME
	 */
	public ArrayList<String> allDepartmentPermission(Integer moduleCode, String personId, String leadUnit, Integer moduleItemKey);
	
	
	/** Check a person has right in a department
	 * 
	 * @param rightName
	 * @param personId
	 * @param unitNumber
	 * @return boolean
	 */
	public boolean isPersonHasRightInDepartment(String rightName , String personId , String unitNumber);
	
	
	/** Check a person has right in a document
	 * 
	 * @param rightName
	 * @param personId
	 * @param moduleCode
	 * @param moduleItemKey
	 * @return
	 */
	public boolean isPersonHasRightInaDocument(String rightName , String personId , Integer moduleCode, String moduleItemKey);
	
	/** Check a person has right in any department
	 * 
	 * @param rightName
	 * @param personId
	 * @param unitNumber
	 * @return boolean
	 */
	public boolean isPersonHasRightInAnyDepartment(String rightName , String personId);	

}
