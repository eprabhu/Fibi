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
@Table(name = "ORCID_WORK_EXTERNAL_IDENTIFIER")
public class OrcidWorkExternalIdentifier implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ORCID_WORK_EXTERNAL_IDENTIFIER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ORCID_WORK_EXTERNAL_IDENTIFIER_ID_GENERATOR")
	@SequenceGenerator(name="ORCID_WORK_EXTERNAL_IDENTIFIER_ID_GENERATOR", sequenceName = "ORCID_WORK_EXTERNAL_IDENTIFIER_ID_GENERATOR", allocationSize=1)
	private Integer orcidWorkIdentifierId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WORK_EXTERNAL_IDENTIFIER_FK1"), name = "PUT_CODE", referencedColumnName = "PUT_CODE")
	private OrcidWork orcidWork;

	@Column(name = "IDENTIFIER_TYPE")
	private String identifierType;

	@Column(name = "IDENTIFIER_VALUE")
	private String identifierValue;

	@Column(name = "IDENTIFIER_URL")
	private String identifierUrl;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getOrcidWorkIdentifierId() {
		return orcidWorkIdentifierId;
	}

	public void setOrcidWorkIdentifierId(Integer orcidWorkIdentifierId) {
		this.orcidWorkIdentifierId = orcidWorkIdentifierId;
	}

	public OrcidWork getOrcidWork() {
		return orcidWork;
	}

	public void setOrcidWork(OrcidWork orcidWork) {
		this.orcidWork = orcidWork;
	}

	public String getIdentifierType() {
		return identifierType;
	}

	public void setIdentifierType(String identifierType) {
		this.identifierType = identifierType;
	}

	public String getIdentifierValue() {
		return identifierValue;
	}

	public void setIdentifierValue(String identifierValue) {
		this.identifierValue = identifierValue;
	}

	public String getIdentifierUrl() {
		return identifierUrl;
	}

	public void setIdentifierUrl(String identifierUrl) {
		this.identifierUrl = identifierUrl;
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
