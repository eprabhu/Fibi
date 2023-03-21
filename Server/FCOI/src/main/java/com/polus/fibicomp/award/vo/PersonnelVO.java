package com.polus.fibicomp.award.vo;

import java.util.List;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonRoles;
import com.polus.fibicomp.award.pojo.AwardPersonUnit;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.pojo.Rolodex;
import com.polus.fibicomp.roles.pojo.ModuleDerivedRoles;

public class PersonnelVO {

	private Integer awardId;

	private String personId;

	private String acType;

	private String message;

	private Award award;

	private AwardPerson awardPerson;

	private AwardPersonUnit awardPersonUnit;

	private AwardContact awardContact;

	private AwardProjectTeam awardProjectTeam;

	private Rolodex rolodex;

	private List<AwardPerson> awardPersons;

	private List<AwardProjectTeam> awardProjectTeams;

	private List<AwardContact> awardContacts;

	private Integer awardPersonalId;

	private String updateUser;

	private Integer awardPersonUnitId;

	private String awardPIPerson;

	private String awardPIPersonId;

	private AwardPersonRoles awardPersonRole;

	private List<AwardPersonRoles> awardPersonRoles;
	
	private Integer awardPersonRoleId;

	private String leadUnitName;

	private String leadUnitNumber;

	private List<ModuleDerivedRoles> moduleDerivedRoles;

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public AwardPerson getAwardPerson() {
		return awardPerson;
	}

	public void setAwardPerson(AwardPerson awardPerson) {
		this.awardPerson = awardPerson;
	}

	public AwardContact getAwardContact() {
		return awardContact;
	}

	public void setAwardContact(AwardContact awardContact) {
		this.awardContact = awardContact;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public AwardProjectTeam getAwardProjectTeam() {
		return awardProjectTeam;
	}

	public void setAwardProjectTeam(AwardProjectTeam awardProjectTeam) {
		this.awardProjectTeam = awardProjectTeam;
	}

	public AwardPersonUnit getAwardPersonUnit() {
		return awardPersonUnit;
	}

	public void setAwardPersonUnit(AwardPersonUnit awardPersonUnit) {
		this.awardPersonUnit = awardPersonUnit;
	}

	public Rolodex getRolodex() {
		return rolodex;
	}

	public void setRolodex(Rolodex rolodex) {
		this.rolodex = rolodex;
	}

	public List<AwardPerson> getAwardPersons() {
		return awardPersons;
	}

	public void setAwardPersons(List<AwardPerson> awardPersons) {
		this.awardPersons = awardPersons;
	}

	public List<AwardProjectTeam> getAwardProjectTeams() {
		return awardProjectTeams;
	}

	public void setAwardProjectTeams(List<AwardProjectTeam> awardProjectTeams) {
		this.awardProjectTeams = awardProjectTeams;
	}

	public List<AwardContact> getAwardContacts() {
		return awardContacts;
	}

	public void setAwardContacts(List<AwardContact> awardContacts) {
		this.awardContacts = awardContacts;
	}

	public Integer getAwardPersonalId() {
		return awardPersonalId;
	}

	public void setAwardPersonalId(Integer awardPersonalId) {
		this.awardPersonalId = awardPersonalId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getAwardPersonUnitId() {
		return awardPersonUnitId;
	}

	public void setAwardPersonUnitId(Integer awardPersonUnitId) {
		this.awardPersonUnitId = awardPersonUnitId;
	}

	public String getAwardPIPerson() {
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson awardPerson : awardPersons) {
				if (awardPerson.getPersonRoleId() == Constants.PI_ROLE_CODE || awardPerson.getPersonRoleId() == Constants.PI_ROLE_CODE_MULTI) {
					awardPIPerson = awardPerson.getFullName();
				}
			}
		}
		return awardPIPerson;
	}

	public void setAwardPIPerson(String awardPIPerson) {
		this.awardPIPerson = awardPIPerson;
	}

	public AwardPersonRoles getAwardPersonRole() {
		return awardPersonRole;
	}

	public void setAwardPersonRole(AwardPersonRoles awardPersonRole) {
		this.awardPersonRole = awardPersonRole;
	}

	public List<AwardPersonRoles> getAwardPersonRoles() {
		return awardPersonRoles;
	}

	public void setAwardPersonRoles(List<AwardPersonRoles> awardPersonRoles) {
		this.awardPersonRoles = awardPersonRoles;
	}

	public Integer getAwardPersonRoleId() {
		return awardPersonRoleId;
	}

	public void setAwardPersonRoleId(Integer awardPersonRoleId) {
		this.awardPersonRoleId = awardPersonRoleId;
	}

	public String getAwardPIPersonId() {
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson awardPerson : awardPersons) {
				if (awardPerson.getPersonRoleId() == Constants.PI_ROLE_CODE || awardPerson.getPersonRoleId() == Constants.PI_ROLE_CODE_MULTI) {
					if (awardPerson.getPersonId() != null) {
						awardPIPersonId = awardPerson.getPersonId();
					}
				}
			}
		}
		return awardPIPersonId;
	}

	public void setAwardPIPersonId(String awardPIPersonId) {
		this.awardPIPersonId = awardPIPersonId;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public String getLeadUnitName() {
		if (award != null && award.getLeadUnit() != null) {
			leadUnitName = award.getLeadUnit().getUnitName();
		}
		return leadUnitName;
	}

	public void setLeadUnitName(String leadUnitName) {
		this.leadUnitName = leadUnitName;
	}

	public String getLeadUnitNumber() {
		if (award != null && award.getLeadUnitNumber() != null) {
			leadUnitNumber = award.getLeadUnitNumber();
		}
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public List<ModuleDerivedRoles> getModuleDerivedRoles() {
		return moduleDerivedRoles;
	}

	public void setModuleDerivedRoles(List<ModuleDerivedRoles> moduleDerivedRoles) {
		this.moduleDerivedRoles = moduleDerivedRoles;
	}

}
