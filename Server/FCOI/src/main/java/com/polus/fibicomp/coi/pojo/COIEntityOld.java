package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

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
import javax.persistence.Table;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.pojo.Country;
import com.polus.fibicomp.pojo.States;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "ENTITY_OLD")
@EntityListeners(AuditingEntityListener.class)
public class COIEntityOld implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ENTITY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer CoiEntityId;

	@Column(name = "ENTITY_NAME")
	private String CoiEntityName;

	@Column(name = "ENTITY_STATUS_CODE")
	private String entityStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FK5"), name = "ENTITY_STATUS_CODE", referencedColumnName = "ENTITY_STATUS_CODE", insertable = false, updatable = false)
	private EntityStatus entityStatus;
	
	@Column(name = "RISK_CATEGORY_CODE")
	private String riskCategoryCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FK1"), name = "RISK_CATEGORY_CODE", referencedColumnName = "RISK_CATEGORY_CODE", insertable = false, updatable = false)
	private EntityRiskCategoryCode entityRiskCategoryCode;

	@Column(name = "ENTITY_TYPE_CODE")
	private String entityTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FK2"), name = "ENTITY_TYPE_CODE", referencedColumnName = "ENTITY_TYPE_CODE", insertable = false, updatable = false)
	private EntityType entityType;

	@Column(name = "COUNTRY_CODE")
	private String countryCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FK3"), name = "COUNTRY_CODE", referencedColumnName = "COUNTRY_CODE", insertable = false, updatable = false)
	private Country country;

	@Column(name = "STATE_CODE")
	private String stateCode;
	
	@Column(name = "PHONE")
	private String phone;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FK4"), name = "STATE_CODE", referencedColumnName = "STATE_CODE", insertable = false, updatable = false)
	private States state;

	@Column(name = "ADDRESS")
	private String address;

	@Column(name = "ZIP_CODE")
	private String zipCode;
	
	@Column(name = "VERSION_NUMBER")
	private String versionNumber;
	
	@Column(name = "VERSION_STATUS")
	private String versionStatus;

	@Column(name = "EMAIL_ADDRESS")
	private String emailAddress;

	@Column(name = "WEB_URL")
	private String webUrl;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "APPROVED_TIMESTAMP")
	private Date approvedTimestamp;

	@Column(name = "APPROVED_USER")
	private String approvedUser;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public Integer getCoiEntityId() {
		return CoiEntityId;
	}

	public void setCoiEntityId(Integer CoiEntityId) {
		this.CoiEntityId = CoiEntityId;
	}

	public String getCoiEntityName() {
		return CoiEntityName;
	}

	public void setCoiEntityName(String CoiEntityName) {
		this.CoiEntityName = CoiEntityName;
	}

	public String getEntityStatusCode() {
		return entityStatusCode;
	}

	public void setEntityStatusCode(String entityStatusCode) {
		this.entityStatusCode = entityStatusCode;
	}

	public EntityStatus getEntityStatus() {
		return entityStatus;
	}

	public void setEntityStatus(EntityStatus entityStatus) {
		this.entityStatus = entityStatus;
	}

	public String getEntityTypeCode() {
		return entityTypeCode;
	}

	public void setEntityTypeCode(String entityTypeCode) {
		this.entityTypeCode = entityTypeCode;
	}

	public EntityType getEntityType() {
		return entityType;
	}

	public void setEntityType(EntityType entityType) {
		this.entityType = entityType;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public Country getCountry() {
		return country;
	}

	public void setCountry(Country country) {
		this.country = country;
	}

	public String getStateCode() {
		return stateCode;
	}

	public void setStateCode(String stateCode) {
		this.stateCode = stateCode;
	}

	public States getState() {
		return state;
	}

	public void setState(States state) {
		this.state = state;
	}

	public String getZipCode() {
		return zipCode;
	}

	public void setZipCode(String zipCode) {
		this.zipCode = zipCode;
	}

	public String getEmailAddress() {
		return emailAddress;
	}

	public void setEmailAddress(String emailAddress) {
		this.emailAddress = emailAddress;
	}

	public String getWebUrl() {
		return webUrl;
	}

	public void setWebUrl(String webUrl) {
		this.webUrl = webUrl;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
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

	public Date getApprovedTimestamp() {
		return approvedTimestamp;
	}

	public void setApprovedTimestamp(Date approvedTimestamp) {
		this.approvedTimestamp = approvedTimestamp;
	}

	public String getApprovedUser() {
		return approvedUser;
	}

	public void setApprovedUser(String approvedUser) {
		this.approvedUser = approvedUser;
	}

	public String getRiskCategoryCode() {
		return riskCategoryCode;
	}

	public void setRiskCategoryCode(String riskCategoryCode) {
		this.riskCategoryCode = riskCategoryCode;
	}

	public EntityRiskCategoryCode getEntityRiskCategoryCode() {
		return entityRiskCategoryCode;
	}

	public void setEntityRiskCategoryCode(EntityRiskCategoryCode entityRiskCategoryCode) {
		this.entityRiskCategoryCode = entityRiskCategoryCode;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public String getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(String versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

}
