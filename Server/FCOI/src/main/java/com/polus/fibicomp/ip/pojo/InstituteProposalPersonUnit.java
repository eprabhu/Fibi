package com.polus.fibicomp.ip.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PROP_PERSON_UNITS")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposalPersonUnit implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROP_PERSON_UNIT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_IP_PERSON_UNIT_ID_GNTR")
	@SequenceGenerator(name="SEQ_IP_PERSON_UNIT_ID_GNTR", sequenceName = "SEQ_IP_PERSON_UNIT_ID_GNTR", allocationSize=1)
	private Integer propPersonUnitId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROP_PERSON_UNITS_FK1"), name = "PROPOSAL_PERSON_ID", referencedColumnName = "PROPOSAL_PERSON_ID")
	private InstituteProposalPerson instProposalPerson;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "LEAD_UNIT_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean leadUnit = false;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "PROP_PERSON_UNITS_FK2"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "PROPOSAL_NUMBER")
	private String proposalNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private Boolean isDeleted = false; 

	public Integer getPropPersonUnitId() {
		return propPersonUnitId;
	}

	public void setPropPersonUnitId(Integer propPersonUnitId) {
		this.propPersonUnitId = propPersonUnitId;
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

	public String getProposalNumber() {
		return proposalNumber;
	}

	public void setProposalNumber(String proposalNumber) {
		this.proposalNumber = proposalNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
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

	public InstituteProposalPerson getInstProposalPerson() {
		return instProposalPerson;
	}

	public void setInstProposalPerson(InstituteProposalPerson instProposalPerson) {
		this.instProposalPerson = instProposalPerson;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Boolean getIsDeleted() {
		return isDeleted;
	}

	public void setIsDeleted(Boolean isDeleted) {
		this.isDeleted = isDeleted;
	}

}
