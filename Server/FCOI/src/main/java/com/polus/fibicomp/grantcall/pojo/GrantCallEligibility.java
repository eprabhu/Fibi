package com.polus.fibicomp.grantcall.pojo;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.pojo.ProposalPersonRole;

@Entity
@Table(name = "GRANT_CALL_ELIGIBILITY")
public class GrantCallEligibility implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_ELIGIBILITY_ID", updatable = false, nullable = false)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_ELIGIBILITY_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_ELIGIBILITY_ID_GENERATOR", sequenceName = "GRANT_ELIGIBILITY_ID_GENERATOR", allocationSize=1)
	private Integer grantEligibilityId;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	@Column(name = "PROP_PERSON_ROLE_ID")
	private Integer personId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_ELGIBILITY_FK2"), name = "PROP_PERSON_ROLE_ID", referencedColumnName = "PROP_PERSON_ROLE_ID", insertable = false, updatable = false)
	private ProposalPersonRole proposalPersonRole;

	@Column(name = "GRANT_ELGBLTY_TYPE_CODE")
	private Integer grantEligibilityTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_ELGIBILITY_FK3"), name = "GRANT_ELGBLTY_TYPE_CODE", referencedColumnName = "GRANT_ELGBLTY_TYPE_CODE", insertable = false, updatable = false)
	private GrantCallEligibilityType grantCallEligibilityType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToOne(mappedBy = "grantCallEligibility", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private GrantEligibilityTarget grantEligibilityTarget;

	public Integer getGrantEligibilityId() {
		return grantEligibilityId;
	}

	public void setGrantEligibilityId(Integer grantEligibilityId) {
		this.grantEligibilityId = grantEligibilityId;
	}

	public Integer getGrantEligibilityTypeCode() {
		return grantEligibilityTypeCode;
	}

	public void setGrantEligibilityTypeCode(Integer grantEligibilityTypeCode) {
		this.grantEligibilityTypeCode = grantEligibilityTypeCode;
	}

	public GrantCallEligibilityType getGrantCallEligibilityType() {
		return grantCallEligibilityType;
	}

	public void setGrantCallEligibilityType(GrantCallEligibilityType grantCallEligibilityType) {
		this.grantCallEligibilityType = grantCallEligibilityType;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public Integer getPersonId() {
		return personId;
	}

	public void setPersonId(Integer personId) {
		this.personId = personId;
	}

	public GrantEligibilityTarget getGrantEligibilityTarget() {
		return grantEligibilityTarget;
	}

	public void setGrantEligibilityTarget(GrantEligibilityTarget grantEligibilityTarget) {
		this.grantEligibilityTarget = grantEligibilityTarget;
	}

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

}
