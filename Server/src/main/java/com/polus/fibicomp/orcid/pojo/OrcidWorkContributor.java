package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "ORCID_WORK_CONTRIBUTOR")
public class OrcidWorkContributor implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ORCID_WORK_CONTRIBUTORS_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ORCID_WORK_CONTRIBUTORS_ID_GENERATOR")
	@SequenceGenerator(name="ORCID_WORK_CONTRIBUTORS_ID_GENERATOR", sequenceName = "ORCID_WORK_CONTRIBUTORS_ID_GENERATOR", allocationSize=1)
	private Integer orcidWorkContributorId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WORK_CONTRIBUTORS_FK1"), name = "PUT_CODE", referencedColumnName = "PUT_CODE")
	private OrcidWork orcidWork;

	@Column(name = "CREDIT_NAME")
	private String creditName;

	@Column(name = "EMAIL_ADDREES")
	private String contributorEmail;

	@Column(name = "CONTRIBUTOR_SEQUENCE")
	private String contributorSequence;

	@Column(name = "CONTRIBUTOR_ROLE")
	private String contributorRole;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getOrcidWorkContributorId() {
		return orcidWorkContributorId;
	}

	public void setOrcidWorkContributorId(Integer orcidWorkContributorId) {
		this.orcidWorkContributorId = orcidWorkContributorId;
	}

	public OrcidWork getOrcidWork() {
		return orcidWork;
	}

	public void setOrcidWork(OrcidWork orcidWork) {
		this.orcidWork = orcidWork;
	}

	public String getCreditName() {
		return creditName;
	}

	public void setCreditName(String creditName) {
		this.creditName = creditName;
	}

	public String getContributorEmail() {
		return contributorEmail;
	}

	public void setContributorEmail(String contributorEmail) {
		this.contributorEmail = contributorEmail;
	}

	public String getContributorSequence() {
		return contributorSequence;
	}

	public void setContributorSequence(String contributorSequence) {
		this.contributorSequence = contributorSequence;
	}

	public String getContributorRole() {
		return contributorRole;
	}

	public void setContributorRole(String contributorRole) {
		this.contributorRole = contributorRole;
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

}
