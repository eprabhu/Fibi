package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.roles.pojo.Role;

@Entity
@Table(name = "ADMIN_GROUP")
public class AdminGroup implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;

	@Column(name = "ADMIN_GROUP_NAME")
	private String adminGroupName;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "EMAIL_ID")
	private String email;

	@Column(name = "ROLE_ID")
	private Integer roleId;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "ADMIN_GROUP_FK1"), name = "ROLE_ID", referencedColumnName = "ROLE_ID", insertable = false, updatable = false)
	private Role role;

	@Column(name = "PERSON_ID")
	private String primaryPersonId;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "ADMIN_GROUP_FK2"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public String getAdminGroupName() {
		return adminGroupName;
	}

	public void setAdminGroupName(String adminGroupName) {
		this.adminGroupName = adminGroupName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getRoleId() {
		return roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}

	public Role getRole() {
		return role;
	}

	public void setRole(Role role) {
		this.role = role;
	}

	public String getPrimaryPersonId() {
		return primaryPersonId;
	}

	public void setPrimaryPersonId(String primaryPersonId) {
		this.primaryPersonId = primaryPersonId;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

}
