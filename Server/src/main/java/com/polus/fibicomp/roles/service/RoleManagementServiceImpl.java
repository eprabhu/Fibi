package com.polus.fibicomp.roles.service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.roles.dao.RolesManagementDao;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.roles.pojo.Rights;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.roles.pojo.RoleRights;
import com.polus.fibicomp.roles.pojo.RoleType;
import com.polus.fibicomp.roles.vo.RoleManagementVO;
import com.polus.fibicomp.roles.vo.RoleRightsVO;
import com.polus.fibicomp.roles.vo.RoleVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@Transactional
@Service(value = "roleManagementService")
public class RoleManagementServiceImpl implements RoleManagementService{

	protected static Logger logger = LogManager.getLogger(RoleManagementServiceImpl.class.getName());

	@Autowired
	private RolesManagementDao rolesManagementDao;
	
	@Autowired
	private CommonDao commonDao;
	
	@Override
	public String getAssignedRoleOfPerson(String personId, String unitNumber) {
		List<RoleVO> roleList = new ArrayList<>();		
		List<PersonRoles> personRolesList = rolesManagementDao.getAssignedRoleOfPerson(personId, unitNumber, null);	
		for(PersonRoles personRole : personRolesList) {
			RoleVO roleVO = new RoleVO();			
			roleVO.setPersonRoleId(personRole.getPersonRoleId());
			roleVO.setRoleId(personRole.getRoleId());
			roleVO.setRoleName(personRole.getRole().getRoleName());
			roleVO.setDescentFlag(personRole.getDescentFlag());
			roleVO.setDepartment(commonDao.getUnitName(unitNumber));
			roleList.add(roleVO);
		}
		return commonDao.convertObjectToJSON(roleList);
	}

	@Override
	public String getUnAssignedRoleOfPerson(String personId, String unitNumber) {
		List<RoleVO> roleList = new ArrayList<>();		
		List<Role> prolesList = rolesManagementDao.getUnAssignedRoleOfPerson(personId, unitNumber);	
		for(Role role : prolesList) {
			RoleVO roleVO = new RoleVO();			
			roleVO.setRoleId(role.getRoleId());
			roleVO.setRoleName(role.getRoleName());
			roleVO.setDescentFlag("N");
			roleList.add(roleVO);
		}	
		return commonDao.convertObjectToJSON(roleList);
	}

	@Override
	public String getRoleInformation(Integer roleId) {		
		Role roleInfo = rolesManagementDao.getRoleInformation(roleId);			
		List<Rights> rightsForRole = rolesManagementDao.getAllRightsForRole(roleId);		
		RoleRightsVO roleRightsVO = new RoleRightsVO();
		roleRightsVO.setRoleId(roleId);
		roleRightsVO.setRoleName(roleInfo.getRoleName());
		roleRightsVO.setRights(rightsForRole);
		return commonDao.convertObjectToJSON(roleRightsVO);		
	}

	@Override
	public String savePersonRoles(RoleManagementVO vo) {
		List<RoleVO> roleList = vo.getRoles();
		String actionName = null;
		for(RoleVO role : roleList) {			
			PersonRoles personRole =  new PersonRoles();
			personRole.setPersonId(vo.getPersonId());
			personRole.setUnitNumber(vo.getUnitNumber());
			personRole.setUpdateUser(vo.getUpdateUser());
			personRole.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			personRole.setDescentFlag(role.getDescentFlag());
			personRole.setRoleId(role.getRoleId());			
			if("I".equals(role.getAcType())) {				
				actionName = "ASSIGN_ROLE";
			}else if("U".equals(role.getAcType())) {
				actionName = "UPDATE_DESCEND_FLAG";
			}else if("D".equals(role.getAcType())) {
				actionName = "DELETE_ROLE";
			}
			rolesManagementDao.syncPersonRole(vo.getPersonId(), vo.getUnitNumber(), role.getRoleId(), null, role.getDescentFlag(), vo.getUpdateUser(), actionName);
			role.setAcType("U");						
		}		
		return commonDao.convertObjectToJSON(roleList);	
	}

	@Override
	public String fetchAllRoles() {
		List<Role> role = rolesManagementDao.fetchAllRoles();
		return commonDao.convertObjectToJSON(role);
	}

	@Override
	public String getAssignedRole(RoleManagementVO vo) {
		return rolesManagementDao.getAssignedRole(vo);
	}

	@Override
	public String createRole(RoleManagementVO vo) {
		List<RoleType> roleTypeList = rolesManagementDao.fetchAllRoleTypes();
		vo.setRoleTypeList(roleTypeList);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateRole(RoleManagementVO vo) {
		Role role = vo.getRole();
		String roleRightAcType = vo.getRoleRightAcType();
		List<Rights> rights = vo.getRoleRights();
		role.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		role.setUpdateUser(vo.getUpdateUser());
		if("I".equals(vo.getAcType())) {
			role.setStatusFlag("Y");
			role.setCreateTimeStamp(commonDao.getCurrentTimestamp());
			role.setCreateUser(vo.getUpdateUser());			
			Integer id = rolesManagementDao.saveRole(role);
			for(Rights right : rights) {
				RoleRights roleRight = new RoleRights();
				roleRight.setRightId(right.getRightId());
				roleRight.setRoleId(id);
				roleRight.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				roleRight.setUpdateUser(vo.getUpdateUser());
				rolesManagementDao.saveRoleRight(roleRight);
			}
			vo.setMessage("New Role saved successfully");
		} else if("U".equals(vo.getAcType())) {
			rolesManagementDao.updateRole(role);
			if (roleRightAcType != null) {
				if(roleRightAcType.equals(Constants.ADD_ROLE_RIGHT)) {
					for(Rights right : rights) {
						rolesManagementDao.syncPersonRole(vo.getPersonId(), vo.getUnitNumber(), role.getRoleId(), right.getRightId(), null, vo.getUpdateUser(), "ASSIGN_RIGHT");
					}
				} else if (roleRightAcType.equals(Constants.DELETE_ROLE_RIGHT)) {
					for(Rights right : rights) {
						rolesManagementDao.syncPersonRole(vo.getPersonId(), vo.getUnitNumber(), role.getRoleId(), right.getRightId(), null, vo.getUpdateUser(), "DELETE_RIGHT");
					}					
				}
			}
			vo.setMessage("Role updated successfully");
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteRole(RoleManagementVO vo) {
		List<RoleRights> roleRights = rolesManagementDao.fetchRoleRightsByRoleId(vo.getRoleId());
		List<PersonRoles> personRoles = rolesManagementDao.getPersonRolesByRoleId(vo.getRoleId());
		rolesManagementDao.deletePersonRoles(personRoles);
		personRoles.forEach(personRole -> rolesManagementDao.roleRIghtAuditTab("PER_ROLES", personRole.getRoleId(),
				null, personRole.getPersonId(), personRole.getUnitNumber(), personRole.getDescentFlag(), AuthenticatedUser.getLoginUserName()));
		roleRights.forEach(roleRight -> rolesManagementDao.roleRIghtAuditTab("ROLE_RIGHTS", roleRight.getRoleId(), 
				roleRight.getRightId(), null, null, null, AuthenticatedUser.getLoginUserName()));
		rolesManagementDao.deleteRoleRights(roleRights);
		rolesManagementDao.deleteRole(vo.getRoleId());
		vo.setMessage("Role deleted successfully");
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAssignedAndUnassignedRights(RoleManagementVO vo) {
		if (vo.getRoleId() != null) {
			vo.setAssignedRights(rolesManagementDao.getAllRightsForRole(vo.getRoleId()));
			vo.setUnAssignedRights(rolesManagementDao.fetchUnAssignedRights(vo.getRoleId()));
		} else {
			vo.setUnAssignedRights(rolesManagementDao.fetchAllRights());
		}		
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getRoleById(RoleManagementVO vo) {
		vo.setRole(rolesManagementDao.getRoleInformation(vo.getRoleId()));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public List<Integer> getDerivedRoleIdForDelete(Integer moduleCode) {
		List<ModuleDerivedRoles> moduleDerivedRoles = rolesManagementDao.grantModuleDerivedRolesForCOI(moduleCode);
		moduleDerivedRoles.addAll(rolesManagementDao.grantModuleDerivedRolesForPI(moduleCode));
		List<Integer> roleIds = new ArrayList<>();
		if(!moduleDerivedRoles.isEmpty()) {
			roleIds = moduleDerivedRoles.stream().map(ModuleDerivedRoles::getRoleId).collect(Collectors.toList()); 
		}
		return roleIds;
	}

}
