package com.polus.fibicomp.proposal.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.roles.pojo.Role;

@Entity
@Table(name = "EPS_PROPOSAL_PERSON_ROLES")
public class ProposalPersonRoles {

	@Id
	@Column(name = "PROPOSAL_PERSON_ROLE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_PERSON_ROLE_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROP_PERSON_ROLE_ID_GENERATOR", sequenceName = "EPS_PROP_PERSON_ROLE_ID_GENERATOR", allocationSize=1)
	private Integer proposalPersonRoleId;

	@Column(name = "PROPOSAL_ID ")
	private Integer proposalId;

	@Column(name = "PERSON_ID")
	private String personId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_PERSON_ROLES_FK1"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", updatable = false, insertable = false)
	private Person person;

	@Column(name = "ROLE_ID")
	private Integer roleId;

	@JsonIgnore
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_PERSON_ROLES_FK2"), name = "ROLE_ID", referencedColumnName = "ROLE_ID", updatable = false, insertable = false)
	private Role role;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String acType;

	@Transient
	private String fullName;

	@Transient
	private String homeUnit;

	@Transient
	private String userName;

	public Integer getProposalPersonRoleId() {
		return proposalPersonRoleId;
	}

	public void setProposalPersonRoleId(Integer proposalPersonRoleId) {
		this.proposalPersonRoleId = proposalPersonRoleId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
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

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getHomeUnit() {
		return homeUnit;
	}

	public void setHomeUnit(String homeUnit) {
		this.homeUnit = homeUnit;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

}
