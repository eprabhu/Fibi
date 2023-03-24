package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
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

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "EPS_PROP_PERSON_UNITS")
public class ProposalPersonUnit implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROP_PERSON_UNIT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_PERSON_UNIT_ID_GENERATOR")
	@SequenceGenerator(name="EPS_PROP_PERSON_UNIT_ID_GENERATOR", sequenceName = "EPS_PROP_PERSON_UNIT_ID_GENERATOR", allocationSize=1)
	private Integer propPersonUnitId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_PERSON_UNITS_FK1"), name = "PROPOSAL_PERSON_ID", referencedColumnName = "PROPOSAL_PERSON_ID")
	private ProposalPerson proposalPerson;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "LEAD_UNIT_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean leadUnit = false;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROP_PERSON_UNITS_FK2"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private boolean isDeleted = false;

	public Integer getPropPersonUnitId() {
		return propPersonUnitId;
	}

	public void setPropPersonUnitId(Integer propPersonUnitId) {
		this.propPersonUnitId = propPersonUnitId;
	}

	public ProposalPerson getProposalPerson() {
		return proposalPerson;
	}

	public void setProposalPerson(ProposalPerson proposalPerson) {
		this.proposalPerson = proposalPerson;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public boolean isLeadUnit() {
		return leadUnit;
	}

	public void setLeadUnit(boolean leadUnit) {
		this.leadUnit = leadUnit;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
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

	public boolean getIsDeleted() {
		return isDeleted;
	}

	public void setIsDeleted(boolean isDeleted) {
		this.isDeleted = isDeleted;
	}

}
