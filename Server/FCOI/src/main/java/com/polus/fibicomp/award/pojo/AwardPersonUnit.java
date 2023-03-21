package com.polus.fibicomp.award.pojo;

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
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_PERSON_UNIT")
public class AwardPersonUnit implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_PERSON_UNIT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardPersonUnitId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PERSON_UNIT_FK2"), name = "AWARD_PERSON_ID", referencedColumnName = "AWARD_PERSON_ID")
	private AwardPerson awardPerson;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PERSON_UNIT_FK3"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "LEAD_UNIT_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean leadUnitFlag;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private Boolean isDeleted = false;

	public Integer getAwardPersonUnitId() {
		return awardPersonUnitId;
	}

	public void setAwardPersonUnitId(Integer awardPersonUnitId) {
		this.awardPersonUnitId = awardPersonUnitId;
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

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Boolean getLeadUnitFlag() {
		return leadUnitFlag;
	}

	public void setLeadUnitFlag(Boolean leadUnitFlag) {
		this.leadUnitFlag = leadUnitFlag;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public AwardPerson getAwardPerson() {
		return awardPerson;
	}

	public void setAwardPerson(AwardPerson awardPerson) {
		this.awardPerson = awardPerson;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public Boolean getIsDeleted() {
		return isDeleted;
	}

	public void setIsDeleted(Boolean isDeleted) {
		this.isDeleted = isDeleted;
	}

}
