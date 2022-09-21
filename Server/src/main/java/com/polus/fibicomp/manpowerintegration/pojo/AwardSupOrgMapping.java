package com.polus.fibicomp.manpowerintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_SUP_ORG_MAPPING")
public class AwardSupOrgMapping implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_SUP_ORG_MAPPING_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AWARD_SUP_ORG_MAPPING")
	@SequenceGenerator(name="SEQ_AWARD_SUP_ORG_MAPPING", sequenceName = "SEQ_AWARD_SUP_ORG_MAPPING", allocationSize=1)
	private Integer awardSupOrgMappingId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "IS_LEVEL_TWO_SUP_ORG_UNIT")
	private String isLevelTwoSupOrgUnit;

	@Column(name = "SUPERIOR_SUP_ORG_ID")
	private String superiorSupOrgId;

	@Column(name = "SUP_ORG_ID")
	private String supOrgId;

	@Column(name = "PI_PERSON_ID")
	private String pIPersonId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardSupOrgMappingId() {
		return awardSupOrgMappingId;
	}

	public void setAwardSupOrgMappingId(Integer awardSupOrgMappingId) {
		this.awardSupOrgMappingId = awardSupOrgMappingId;
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

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getSuperiorSupOrgId() {
		return superiorSupOrgId;
	}

	public void setSuperiorSupOrgId(String superiorSupOrgId) {
		this.superiorSupOrgId = superiorSupOrgId;
	}

	public String getSupOrgId() {
		return supOrgId;
	}

	public void setSupOrgId(String supOrgId) {
		this.supOrgId = supOrgId;
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

	public String getpIPersonId() {
		return pIPersonId;
	}

	public void setpIPersonId(String pIPersonId) {
		this.pIPersonId = pIPersonId;
	}

	public String getIsLevelTwoSupOrgUnit() {
		return isLevelTwoSupOrgUnit;
	}

	public void setIsLevelTwoSupOrgUnit(String isLevelTwoSupOrgUnit) {
		this.isLevelTwoSupOrgUnit = isLevelTwoSupOrgUnit;
	}

}
