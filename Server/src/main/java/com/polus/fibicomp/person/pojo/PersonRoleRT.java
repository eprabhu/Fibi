package com.polus.fibicomp.person.pojo;

import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "PERSON_ROLE_RT")
public class PersonRoleRT{

	@EmbeddedId
	private PersonRoleRTAttributes personRoleRTAttributes;

	@Column(name = "ROLE_ID")
	private Integer roleId;

	public PersonRoleRTAttributes getPersonRoleRTAttributes() {
		return personRoleRTAttributes;
	}

	public void setPersonRoleRTAttributes(PersonRoleRTAttributes personRoleRTAttributes) {
		this.personRoleRTAttributes = personRoleRTAttributes;
	}

	public Integer getRoleId() {
		return roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}

}
