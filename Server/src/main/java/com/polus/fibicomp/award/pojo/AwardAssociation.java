package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "AWARD_ASSOCIATION")
public class AwardAssociation implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_ASSOCIATION_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardAssociationId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "ASSOCIATION_TYPE_CODE")
	private String associationTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_ASSOCIATION_FK2"), name = "ASSOCIATION_TYPE_CODE", referencedColumnName = "ASSOCIATION_TYPE_CODE", insertable = false, updatable = false)
	private AwardAssociationType awardAssociationType;

	@Column(name = "ASSOCIATED_PROJECT_ID")
	private String associatedProjectId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToOne(mappedBy = "awardAssociation", orphanRemoval = true, cascade = {CascadeType.ALL }, fetch = FetchType.LAZY)
	private AwardAssociationDetail awardAssociationDetail;

	public Integer getAwardAssociationId() {
		return awardAssociationId;
	}

	public void setAwardAssociationId(Integer awardAssociationId) {
		this.awardAssociationId = awardAssociationId;
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

	public String getAssociationTypeCode() {
		return associationTypeCode;
	}

	public void setAssociationTypeCode(String associationTypeCode) {
		this.associationTypeCode = associationTypeCode;
	}

	public AwardAssociationType getAwardAssociationType() {
		return awardAssociationType;
	}

	public void setAwardAssociationType(AwardAssociationType awardAssociationType) {
		this.awardAssociationType = awardAssociationType;
	}

	public String getAssociatedProjectId() {
		return associatedProjectId;
	}

	public void setAssociatedProjectId(String associatedProjectId) {
		this.associatedProjectId = associatedProjectId;
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

	public AwardAssociationDetail getAwardAssociationDetail() {
		return awardAssociationDetail;
	}

	public void setAwardAssociationDetail(AwardAssociationDetail awardAssociationDetail) {
		this.awardAssociationDetail = awardAssociationDetail;
	}

}
