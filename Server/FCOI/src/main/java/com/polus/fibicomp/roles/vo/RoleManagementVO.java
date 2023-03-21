package com.polus.fibicomp.roles.vo;

import java.util.List;


import com.polus.fibicomp.roles.pojo.Rights;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.roles.pojo.RoleType;

public class RoleManagementVO{

	private String personId;

	private String unitNumber;

	private List<RoleVO> roles;

	private String updateUser;

	private Integer roleId;

	private List<Rights> assignedRights;

	private List<Rights> unAssignedRights;

	private List<RoleType> roleTypeList;

	private List<Rights> roleRights;

	private Role role;

	private String roleRightAcType;

	private String acType;

	private Integer roleRightId;

	private Integer rightId;

	private String message;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public List<RoleVO> getRoles() {
		return roles;
	}

	public void setRoles(List<RoleVO> roles) {
		this.roles = roles;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getRoleId() {
		return roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}

	public List<RoleType> getRoleTypeList() {
		return roleTypeList;
	}

	public void setRoleTypeList(List<RoleType> roleTypeList) {
		this.roleTypeList = roleTypeList;
	}

	public Role getRole() {
		return role;
	}

	public void setRole(Role role) {
		this.role = role;
	}

	public String getRoleRightAcType() {
		return roleRightAcType;
	}

	public void setRoleRightAcType(String roleRightAcType) {
		this.roleRightAcType = roleRightAcType;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getRoleRightId() {
		return roleRightId;
	}

	public void setRoleRightId(Integer roleRightId) {
		this.roleRightId = roleRightId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<Rights> getAssignedRights() {
		return assignedRights;
	}

	public void setAssignedRights(List<Rights> assignedRights) {
		this.assignedRights = assignedRights;
	}

	public List<Rights> getUnAssignedRights() {
		return unAssignedRights;
	}

	public void setUnAssignedRights(List<Rights> unAssignedRights) {
		this.unAssignedRights = unAssignedRights;
	}

	public Integer getRightId() {
		return rightId;
	}

	public void setRightId(Integer rightId) {
		this.rightId = rightId;
	}

	public List<Rights> getRoleRights() {
		return roleRights;
	}

	public void setRoleRights(List<Rights> roleRights) {
		this.roleRights = roleRights;
	}

}
