package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_PERSON_ROLES")
public class AwardPersonRoles implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_PERSON_ROLE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardPersonRoleId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "PERSON_ID")
	private String personId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PERSON_ROLES_FK1"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", updatable = false, insertable = false)
	private Person person;

	@Column(name = "ROLE_ID")
	private Integer roleId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PERSON_ROLES_FK2"), name = "ROLE_ID", referencedColumnName = "ROLE_ID", updatable = false, insertable = false)
	private Role role;

	@Column(name = "IS_SYSTEM_GENERATED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSystemGenerated;

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

	public Integer getAwardPersonRoleId() {
		return awardPersonRoleId;
	}

	public void setAwardPersonRoleId(Integer awardPersonRoleId) {
		this.awardPersonRoleId = awardPersonRoleId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
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

	public Boolean getIsSystemGenerated() {
		return isSystemGenerated;
	}

	public void setIsSystemGenerated(Boolean isSystemGenerated) {
		this.isSystemGenerated = isSystemGenerated;
	}

}
