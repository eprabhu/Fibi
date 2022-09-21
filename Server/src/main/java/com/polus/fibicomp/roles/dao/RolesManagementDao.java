package com.polus.fibicomp.roles.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.proposal.pojo.ProposalPersonRoles;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.roles.pojo.Rights;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.roles.pojo.RoleRights;
import com.polus.fibicomp.roles.pojo.RoleType;
import com.polus.fibicomp.roles.vo.RoleManagementVO;

@Transactional
@Service
public interface RolesManagementDao {

	public List<PersonRoles> getAssignedRoleOfPerson(String personId, String unitNumber, Integer roleId);

	public List<Role> getUnAssignedRoleOfPerson(String personId, String unitNumber);

	public Role getRoleInformation(Integer roleId);

	public List<Rights> getAllRightsForRole(Integer roleId);

	public Integer insertPersonRole(PersonRoles personRoles);

	public void updatePersonRole(PersonRoles personRoles);

	public void deletePersonRole(PersonRoles personRoles);

	public List<Role> fetchAllRoles();

	public String getAssignedRole(RoleManagementVO vo);

	public List<Rights> fetchAllRights();

	public List<RoleType> fetchAllRoleTypes();

	public Integer saveRole(Role role);

	public void saveRoleRight(RoleRights roleRight);

	public List<RoleRights> fetchRoleRightsByRoleId(Integer roleId);

	public void deleteRoleRight(Integer roleRightId);

	public void deleteRole(Integer roleId);

	public void deleteRoleRights(List<RoleRights> roleRights);

	public void updateRole(Role role);

	public List<Role> getRoleBasedOnRoleId(List<Integer> roleIds);

	public List<Rights> fetchUnAssignedRights(Integer roleId);

	public Integer getRoleRightIdByRoleAndRightId(Integer roleId, Integer rightId);

	public void deletePersonRoles(List<PersonRoles> personRoles);

	public List<PersonRoles> getPersonRolesByRoleId(Integer roleId);

	public Boolean isPersonHasRole(String personId, Integer roleId);

	public Boolean isPersonHasRightInAnyDepartment(String personId, String rightName);

	/**
	 * This method is used to delete proposal  persons based on personRole.
	 * 
	 * @param personRole - List of personRole.
	 * @return success message.
	 */
	public String deleteProposalPersons(List<ProposalPersonRoles> personRole);

	/**
	 * This method is used to sync the assigned roles to the person
	 * @param personId
	 * @param unitNumber
	 * @param roleId
	 * @param rightId
	 * @param descentFlag
	 * @param updateUser
	 * @param acType
	 */
	public void syncPersonRole(String personId, String unitNumber, Integer roleId, Integer rightId, String descentFlag, String updateUser, String acType);

	/**
	 * This method is used to get the derived roles for the given module
	 * @param moduleCode
	 * @return list of derived roles
	 */
	public List<ModuleDerivedRoles> getModuleDerivedRoles(Integer moduleCode);

	/**
	 * This method is used to get the module derived roles for the given module where it can assign to creator
	 * @param moduleCode
	 * @return list of module derived roles
	 */
	public List<ModuleDerivedRoles> grantModuleDerivedRolesForCreator(Integer moduleCode);

	/**
	 * This method is used to get the module derived roles for the given module where it can assign to PI
	 * @param moduleCode
	 * @return list of module derived roles
	 */
	public List<ModuleDerivedRoles> grantModuleDerivedRolesForPI(Integer moduleCode);

	/**
	 * This method is used to get the module derived roles for the given module where it can assign to COI
	 * @param moduleCode
	 * @return list of module derived roles
	 */
	public List<ModuleDerivedRoles> grantModuleDerivedRolesForCOI(Integer moduleCode);

	/**
	 * @param source
	 * @param oldRoleId
	 * @param oldRightId
	 * @param personId
	 * @param oldUnitNumber
	 * @param descentFlag
	 * @param updateUser
	 */
	public void roleRIghtAuditTab(String source, Integer oldRoleId, Integer oldRightId, String personId,
			String oldUnitNumber, String descentFlag, String updateUser);

}
