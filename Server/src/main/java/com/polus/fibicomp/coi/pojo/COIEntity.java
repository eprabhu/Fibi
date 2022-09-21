package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
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

@Entity
@Table(name = "COI_ENTITY")
@EntityListeners(AuditingEntityListener.class)
public class COIEntity implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_ENTITY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer coiEntityId;

	@Column(name = "COI_ENTITY_NAME")
	private String coiEntityName;

	@Column(name = "ENTITY_NAME_GIVEN_BY_CREATOR")
	private String entityNameGivenByCreator;

	@Column(name = "ORIGINAL_ENTITY_ID")
	private Integer originalEntityId;

	@Column(name = "ENTITY_STATUS_CODE")
	private String entityStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FK1"), name = "ENTITY_STATUS_CODE", referencedColumnName = "ENTITY_STATUS_CODE", insertable = false, updatable = false)
	private EntityStatus entityStatus;

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

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_FK4"), name = "STATE_CODE", referencedColumnName = "STATE_CODE", insertable = false, updatable = false)
	private States state;

	@Column(name = "ADDRESS_LINE_1")
	private String addressLine1;

	@Column(name = "ADDRESS_LINE_2")
	private String addressLine2;

	@Column(name = "ADDRESS_LINE_3")
	private String addressLine3;

	@Column(name = "PINCODE")
	private String pincode;

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

	public Integer getCoiEntityId() {
		return coiEntityId;
	}

	public void setCoiEntityId(Integer coiEntityId) {
		this.coiEntityId = coiEntityId;
	}

	public String getCoiEntityName() {
		return coiEntityName;
	}

	public void setCoiEntityName(String coiEntityName) {
		this.coiEntityName = coiEntityName;
	}

	public String getEntityNameGivenByCreator() {
		return entityNameGivenByCreator;
	}

	public void setEntityNameGivenByCreator(String entityNameGivenByCreator) {
		this.entityNameGivenByCreator = entityNameGivenByCreator;
	}

	public Integer getOriginalEntityId() {
		return originalEntityId;
	}

	public void setOriginalEntityId(Integer originalEntityId) {
		this.originalEntityId = originalEntityId;
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

	public String getAddressLine1() {
		return addressLine1;
	}

	public void setAddressLine1(String addressLine1) {
		this.addressLine1 = addressLine1;
	}

	public String getAddressLine2() {
		return addressLine2;
	}

	public void setAddressLine2(String addressLine2) {
		this.addressLine2 = addressLine2;
	}

	public String getAddressLine3() {
		return addressLine3;
	}

	public void setAddressLine3(String addressLine3) {
		this.addressLine3 = addressLine3;
	}

	public String getPincode() {
		return pincode;
	}

	public void setPincode(String pincode) {
		this.pincode = pincode;
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

}
