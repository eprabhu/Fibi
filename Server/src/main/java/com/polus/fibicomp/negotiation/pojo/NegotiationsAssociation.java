package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "NEGOTIATION_ASSOCIATION")
public class NegotiationsAssociation implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_NEGOTIATION_ASSOCIATION")
	@SequenceGenerator(name = "SEQ_NEGOTIATION_ASSOCIATION", sequenceName = "SEQ_NEGOTIATION_ASSOCIATION", allocationSize = 1)
	@Column(name = "NEGOTIATIONS_ASSOCIATION_ID")
	private Integer negotiationsAssociationId;

	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOCIATION_FK1"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;

	@Column(name = "ASSOCIATION_TYPE_CODE")
	private String associationTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ASSOCIATION_FK2"), name = "ASSOCIATION_TYPE_CODE", referencedColumnName = "ASSOCIATION_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsAssociationType negotiationsAssociationType;

	@Column(name = "ASSOCIATED_PROJECT_ID")
	private String associatedProjectId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String acType;

	public Negotiations getNegotiations() {
		return negotiations;
	}

	public void setNegotiations(Negotiations negotiations) {
		this.negotiations = negotiations;
	}

	public String getAssociationTypeCode() {
		return associationTypeCode;
	}

	public void setAssociationTypeCode(String associationTypeCode) {
		this.associationTypeCode = associationTypeCode;
	}

	public NegotiationsAssociationType getNegotiationsAssociationType() {
		return negotiationsAssociationType;
	}

	public void setNegotiationsAssociationType(NegotiationsAssociationType negotiationsAssociationType) {
		this.negotiationsAssociationType = negotiationsAssociationType;
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

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getNegotiationsAssociationId() {
		return negotiationsAssociationId;
	}

	public void setNegotiationsAssociationId(Integer negotiationsAssociationId) {
		this.negotiationsAssociationId = negotiationsAssociationId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

//	public Integer getAssociationNumber() {
//		return associationNumber;
//	}
//
//	public void setAssociationNumber(Integer associationNumber) {
//		this.associationNumber = associationNumber;
//	}

}
