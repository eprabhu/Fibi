package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "UNIT")
public class Unit implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "PARENT_UNIT_NUMBER")
	private String parentUnitNumber;

	@Column(name = "ORGANIZATION_ID")
	private String organizationId;

	@Column(name = "UNIT_NAME")
	private String unitName;

	@Column(name = "ACTIVE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean active;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "ACRONYM")
	private String acronym;

	@Column(name = "IS_FUNDING_UNIT")
	private String isFundingUnit;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "unit", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch=FetchType.EAGER)
	private List<UnitAdministrator> unitAdministrators;

	@Transient
	private String unitDetail;

	@Transient
	private String parentUnitName;

	@Transient
	private String organizationName;

	public String getIsFundingUnit() {
		return isFundingUnit;
	}

	public void setIsFundingUnit(String isFundingUnit) {
		this.isFundingUnit = isFundingUnit;
	}

	public String getOrganizationName() {
		return organizationName;
	}

	public void setOrganizationName(String organizationName) {
		this.organizationName = organizationName;
	}

	public Unit() {
		unitAdministrators = new ArrayList<>();
	}

	public Unit (String unitNumber, String unitName) {
		super();
		this.unitNumber = unitNumber;
		this.unitName = unitName;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getParentUnitNumber() {
		return parentUnitNumber;
	}

	public void setParentUnitNumber(String parentUnitNumber) {
		this.parentUnitNumber = parentUnitNumber;
	}

	public String getOrganizationId() {
		return organizationId;
	}

	public void setOrganizationId(String organizationId) {
		this.organizationId = organizationId;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public boolean isActive() {
		return active;
	}

	public void setActive(boolean active) {
		this.active = active;
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

	public List<UnitAdministrator> getUnitAdministrators() {
		return (List<UnitAdministrator>) unitAdministrators;
	}

	public void setUnitAdministrators(List<UnitAdministrator> unitAdministrators) {
		this.unitAdministrators = (List<UnitAdministrator>) unitAdministrators;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	@Transient
	public String getUnitDetail() {
		if (unitNumber != null && !unitNumber.isEmpty() && unitName != null && !unitName.isEmpty()) {
			unitDetail = unitNumber + " - " + unitName;
		}
		return unitDetail;
	}

	public void setUnitDetail(String unitDetail) {
		this.unitDetail = unitDetail;
	}

	public String getParentUnitName() {
		return parentUnitName;
	}

	public void setParentUnitName(String parentUnitName) {
		this.parentUnitName = parentUnitName;
	}

	public String getAcronym() {
		return acronym;
	}

	public void setAcronym(String acronym) {
		this.acronym = acronym;
	}

}
