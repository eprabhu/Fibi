package com.polus.fibicomp.roles.service;

import java.util.List;

import com.polus.fibicomp.roles.vo.RoleManagementVO;

public interface RoleManagementService {


	/**
	 * This method will fetch all assigned roles of a person
	 * @param String - Person Id
	 * @param String - Unit Number
	 * @return String - JSON String.
	 */
	String getAssignedRoleOfPerson(String personId,String unitNumber);
	
	/**
	 * This method will fetch all unassigned roles of a person
	 * @param String - Person Id
	 * @param String - Unit Number
	 * @return String - JSON String.
	 */
	String getUnAssignedRoleOfPerson(String personId,String unitNumber);
	
	/**
	 * This method return all the information of a Role
	 * @param Integer - Role Id
	 * @return String - JSON String.
	 */
	String getRoleInformation(Integer roleId);

	/**
	 * This method return assignee/update/delete role to a person
	 * @param RoleManagementVO - Role information VO
	 * @return String - JSON String.
	 */
	String savePersonRoles(RoleManagementVO vo);

	/**
	 * This method return fetchAllRoles
	 * @return String - JSON String.
	 */

	public String fetchAllRoles();

	/**
	 * This method return fetch Assigned Role based on criteria
	 * @param RoleManagementVO - Role information VO
	 * @return String - JSON String.
	 */

	public String getAssignedRole(RoleManagementVO vo);

	/**
	 * This method is used to create new role.
	 * @param vo - Role information VO
	 * @return String - JSON data with roleTypeList and rightList
	 */
	public String createRole(RoleManagementVO vo);

	/**
	 * This method is used to save or edit a role.
	 * @param vo - Role information VO
	 * @return String - JSON String.
	 */
	public String saveOrUpdateRole(RoleManagementVO vo);

	/**
	 * This method is used to delete a role.
	 * @param vo - Role information VO
	 * @return String - JSON String.
	 */
	public String deleteRole(RoleManagementVO vo);

	/**
	 * This method is used to get the assigned and un-assigned rights
	 * @param vo
	 * @return list of rights
	 */
	public String getAssignedAndUnassignedRights(RoleManagementVO vo);

	/**
	 * This method is used to get role by role id
	 * @param vo
	 * @return details of the role
	 */
	public String getRoleById(RoleManagementVO vo);

	/**
	 * This method is used to get the roleId of module derived roles based on module code
	 * @param moduleCode
	 * @return list of derived role ids
	 */
	public List<Integer> getDerivedRoleIdForDelete(Integer moduleCode);

}
