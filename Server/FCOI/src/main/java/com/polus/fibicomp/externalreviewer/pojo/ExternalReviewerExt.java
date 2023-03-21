package com.polus.fibicomp.externalreviewer.pojo;

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
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "EXTERNAL_REVIEWER_EXT")
public class ExternalReviewerExt implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EXTERNAL_REVIEWER_EXT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer externalReviewerExtId;
	
	@Column(name = "EXTERNAL_REVIEWER_ID")
	private Integer externalReviewerId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_EXT_FK1"), name = "EXTERNAL_REVIEWER_ID", referencedColumnName = "EXTERNAL_REVIEWER_ID", insertable = false, updatable = false)
	private ExternalReviewer externalReviewer;

	@Column(name = "HI_INDEX")
	private Integer hIndex;

	@Column(name = "SCOPUS_URL")
	private String scopusUrl;

	@Column(name = "COI_WITH_PERSON")
	private String coiWithPerson;

	@Column(name = "SUPPLIER_DOF")
	private String supplierDof;

	@Column(name = "CIRA_CODE")
	private String ciraCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_EXT_FK2"), name = "CIRA_CODE", referencedColumnName = "CIRA_CODE", insertable = false, updatable = false)
	private ExtReviewerCira extReviewerCira;
	
	@Column(name = "ORGINALITY_CODE")
	private String orginalityCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_EXT_FK3"), name = "ORGINALITY_CODE", referencedColumnName = "ORGINALITY_CODE", insertable = false, updatable = false)
	private ExtReviewerOriginality extReviewerOriginality;

	@Column(name = "THOROUGHNESS_CODE")
	private String thoroughnessCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "EXTERNAL_REVIEWER_EXT_FK4"), name = "THOROUGHNESS_CODE", referencedColumnName = "THOROUGHNESS_CODE", insertable = false, updatable = false)
	private ExtReviewerThoroughness extReviewerThoroughness;

	@JsonIgnore
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@JsonIgnore
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "URL_PROFILE")
	private String urlProfile;

	@Column(name = "DISCIPLINARY_FIELD")
	private String disciplinaryField;

	public Integer getExternalReviewerExtId() {
		return externalReviewerExtId;
	}

	public void setExternalReviewerExtId(Integer externalReviewerExtId) {
		this.externalReviewerExtId = externalReviewerExtId;
	}

	public Integer getExternalReviewerId() {
		return externalReviewerId;
	}

	public void setExternalReviewerId(Integer externalReviewerId) {
		this.externalReviewerId = externalReviewerId;
	}

	public ExternalReviewer getExternalReviewer() {
		return externalReviewer;
	}

	public void setExternalReviewer(ExternalReviewer externalReviewer) {
		this.externalReviewer = externalReviewer;
	}

	public Integer gethIndex() {
		return hIndex;
	}

	public void sethIndex(Integer hIndex) {
		this.hIndex = hIndex;
	}

	public String getScopusUrl() {
		return scopusUrl;
	}

	public void setScopusUrl(String scopusUrl) {
		this.scopusUrl = scopusUrl;
	}

	public String getCoiWithPerson() {
		return coiWithPerson;
	}

	public void setCoiWithPerson(String coiWithPerson) {
		this.coiWithPerson = coiWithPerson;
	}

	public String getSupplierDof() {
		return supplierDof;
	}

	public void setSupplierDof(String supplierDof) {
		this.supplierDof = supplierDof;
	}

	public String getCiraCode() {
		return ciraCode;
	}

	public void setCiraCode(String ciraCode) {
		this.ciraCode = ciraCode;
	}

	public ExtReviewerCira getExtReviewerCira() {
		return extReviewerCira;
	}

	public void setExtReviewerCira(ExtReviewerCira extReviewerCira) {
		this.extReviewerCira = extReviewerCira;
	}

	public String getOrginalityCode() {
		return orginalityCode;
	}

	public void setOrginalityCode(String orginalityCode) {
		this.orginalityCode = orginalityCode;
	}

	public ExtReviewerOriginality getExtReviewerOriginality() {
		return extReviewerOriginality;
	}

	public void setExtReviewerOriginality(ExtReviewerOriginality extReviewerOriginality) {
		this.extReviewerOriginality = extReviewerOriginality;
	}

	public String getThoroughnessCode() {
		return thoroughnessCode;
	}

	public void setThoroughnessCode(String thoroughnessCode) {
		this.thoroughnessCode = thoroughnessCode;
	}

	public ExtReviewerThoroughness getExtReviewerThoroughness() {
		return extReviewerThoroughness;
	}

	public void setExtReviewerThoroughness(ExtReviewerThoroughness extReviewerThoroughness) {
		this.extReviewerThoroughness = extReviewerThoroughness;
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

	public String getUrlProfile() {
		return urlProfile;
	}

	public void setUrlProfile(String urlProfile) {
		this.urlProfile = urlProfile;
	}

	public String getDisciplinaryField() {
		return disciplinaryField;
	}

	public void setDisciplinaryField(String disciplinaryField) {
		this.disciplinaryField = disciplinaryField;
	}

}