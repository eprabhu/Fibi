package com.polus.fibicomp.roles.vo;

public class RoleVO {

	private Integer roleId;
	
	private String roleName;
	
	private String description;
	
	private String descentFlag;
	
	private String roleTypeCode;
	
	private Integer personRoleId;
	
	private String acType;

	private String department;

	public Integer getRoleId() {
		return roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}

	public String getRoleName() {
		return roleName;
	}

	public void setRoleName(String roleName) {
		this.roleName = roleName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getDescentFlag() {
		return descentFlag;
	}

	public void setDescentFlag(String descentFlag) {
		this.descentFlag = descentFlag;
	}

	public String getRoleTypeCode() {
		return roleTypeCode;
	}

	public void setRoleTypeCode(String roleTypeCode) {
		this.roleTypeCode = roleTypeCode;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}
		
}
