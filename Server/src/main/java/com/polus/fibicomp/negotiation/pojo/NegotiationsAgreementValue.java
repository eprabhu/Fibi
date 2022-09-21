package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "NEGOTIATION_AGREEMENT_VALUE")
public class NegotiationsAgreementValue implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "negotiationAgreementCodeGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "negotiationAgreementCodeGenerator")
	@Column(name = "NEGOTIATIONS_AGREEMENT_ID")
	private Integer negotiationsAgreementId;

	@Column(name = "NEGOTIATIONS_ASSOCIATION_ID")
	private Integer negotiationsAssociationId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_AGRMNT_VALUE_FK1"), name = "NEGOTIATIONS_ASSOCIATION_ID", referencedColumnName = "NEGOTIATIONS_ASSOCIATION_ID", insertable = false, updatable = false)
	private NegotiationsAssociation negotiationsAssociation;

	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_AGRMNT_VALUE_FK2"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;

	@Column(name = "PERIOD_NUMBER")
	private Integer periodNumber;

	@Column(name = "SPONSOR_DISTRIBUTION_AMT")
	private Integer sponsorDistributionAmount;

	@Column(name = "INSTITUTION_DISTRIBUTION_AMT")
	private Integer institutionDistributionAmount;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String acType;

	public NegotiationsAssociation getNegotiationsAssociation() {
		return negotiationsAssociation;
	}

	public void setNegotiationsAssociation(NegotiationsAssociation negotiationsAssociation) {
		this.negotiationsAssociation = negotiationsAssociation;
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
		if (acType == null) {
			return "U";
		}
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Negotiations getNegotiations() {
		return negotiations;
	}

	public void setNegotiations(Negotiations negotiations) {
		this.negotiations = negotiations;
	}

	public Integer getNegotiationsAgreementId() {
		return negotiationsAgreementId;
	}

	public void setNegotiationsAgreementId(Integer negotiationsAgreementId) {
		this.negotiationsAgreementId = negotiationsAgreementId;
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

	public Integer getPeriodNumber() {
		return periodNumber;
	}

	public void setPeriodNumber(Integer periodNumber) {
		this.periodNumber = periodNumber;
	}

	public Integer getSponsorDistributionAmount() {
		return sponsorDistributionAmount;
	}

	public void setSponsorDistributionAmount(Integer sponsorDistributionAmount) {
		this.sponsorDistributionAmount = sponsorDistributionAmount;
	}

	public Integer getInstitutionDistributionAmount() {
		return institutionDistributionAmount;
	}

	public void setInstitutionDistributionAmount(Integer institutionDistributionAmount) {
		this.institutionDistributionAmount = institutionDistributionAmount;
	}

}
