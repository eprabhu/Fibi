package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
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

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.pojo.Organization;

@Entity
@Table(name = "AWARD_APPROVED_SUBAWARDS")
@EntityListeners(AuditingEntityListener.class)
public class AwardSubContract implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_APPROVED_SUBAWARD_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_APPROVED_SUBAWARD_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_APPROVED_SUBAWARD_ID_GENERATOR", sequenceName = "AWARD_APPROVED_SUBAWARD_ID_GENERATOR", allocationSize=1)
	private Integer awardApprovedSubawardId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "ORGANIZATION_ID")
	private String organizationId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_APPROVED_SUBAWARDS_FK2"), name = "ORGANIZATION_ID", referencedColumnName = "ORGANIZATION_ID", insertable = false, updatable = false)
	private Organization organization;

	@Column(name = "AMOUNT")
	private Double amount;

	@CreatedBy
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@CreatedDate
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getAwardApprovedSubawardId() {
		return awardApprovedSubawardId;
	}

	public void setAwardApprovedSubawardId(Integer awardApprovedSubawardId) {
		this.awardApprovedSubawardId = awardApprovedSubawardId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

/*	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}*/

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

	public String getOrganizationId() {
		return organizationId;
	}

	public void setOrganizationId(String organizationId) {
		this.organizationId = organizationId;
	}

	public Organization getOrganization() {
		return organization;
	}

	public void setOrganization(Organization organization) {
		this.organization = organization;
	}

	public Double getAmount() {
		return amount;
	}

	public void setAmount(Double amount) {
		this.amount = amount;
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

}
