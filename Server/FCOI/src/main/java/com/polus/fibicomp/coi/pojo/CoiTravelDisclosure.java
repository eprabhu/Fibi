package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
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

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COI_TRAVEL_DISCLOSURE")
@EntityListeners(AuditingEntityListener.class)
public class CoiTravelDisclosure implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TRAVEL_DISCLOSURE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer travelDisclosureId;
	
	@Column(name = "TRAVEL_NUMBER")
	private String travelNumber;
	
	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;
	
	@Column(name = "VERSION_STATUS")
	private String versionStatus;
	
	@Column(name = "PERSON_ENTITY_ID")
	private Integer personEntityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK1"), name = "PERSON_ENTITY_ID", referencedColumnName = "PERSON_ENTITY_ID", insertable = false, updatable = false)
	private PersonEntity personEntity;
	
	@Column(name = "ENTITY_ID")
	private Integer entityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK2"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
	private CoiEntity CoiEntity;
	
	@Column(name = "ENTITY_NUMBER")
	private Integer entityNumber;
	
	@Column(name = "PERSON_ID")
	private String personId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK3"), name = "PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;
	
	@Column(name = "TRAVEL_STATUS_CODE")
	private String travelStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK4"), name = "TRAVEL_STATUS_CODE", referencedColumnName = "TRAVEL_STATUS_CODE", insertable = false, updatable = false)
	private CoiTravelerStatusType coiTravelerStatusType;
	
	@Column(name = "IS_SPONSORED_TRAVEL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSponsoredTravel;
	
	@Column(name = "TRAVEL_TITLE")
	private String travelTitle;
	
	@Column(name = "PURPOSE_OF_THE_TRIP")
	private String purposeOfTheTrip;
	
	@Column(name = "TRAVEL_AMOUNT", precision = 12, scale = 2)
	private BigDecimal travelAmount;
	
	@Column(name = "TRAVEL_START_DATE")
	private Date travelStartDate;
	
	@Column(name = "NO_OF_DAYS")
	private Integer noOfDays;
	
	@Column(name = "DESTINATION_CITY")
	private String destinationCity;
	
	@Column(name = "DESTINATION_COUNTRY")
	private String destinationCountry;
	
	@Column(name = "RELATIONSHIP_TO_YOUR_RESEARCH")
	private String relationshipToYourResearch;
	
	@Column(name = "ACKNOWLEDGE_BY")
	private String acknowledgeBy;
	
	@Column(name = "ACKNOWLEDGE_AT")
	private String acknowledgeAt;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;
	
	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public String getTravelNumber() {
		return travelNumber;
	}

	public void setTravelNumber(String travelNumber) {
		this.travelNumber = travelNumber;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public Integer getPersonEntityId() {
		return personEntityId;
	}

	public void setPersonEntityId(Integer personEntityId) {
		this.personEntityId = personEntityId;
	}

	public PersonEntity getPersonEntity() {
		return personEntity;
	}

	public void setPersonEntity(PersonEntity personEntity) {
		this.personEntity = personEntity;
	}

	public Integer getEntityId() {
		return entityId;
	}

	public void setEntityId(Integer entityId) {
		this.entityId = entityId;
	}

	public CoiEntity getCoiEntity() {
		return CoiEntity;
	}

	public void setCoiEntity(CoiEntity CoiEntity) {
		this.CoiEntity = CoiEntity;
	}

	public Integer getEntityNumber() {
		return entityNumber;
	}

	public void setEntityNumber(Integer entityNumber) {
		this.entityNumber = entityNumber;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public String getTravelStatusCode() {
		return travelStatusCode;
	}

	public void setTravelStatusCode(String travelStatusCode) {
		this.travelStatusCode = travelStatusCode;
	}

	public CoiTravelerStatusType getCoiTravelerStatusType() {
		return coiTravelerStatusType;
	}

	public void setCoiTravelerStatusType(CoiTravelerStatusType coiTravelerStatusType) {
		this.coiTravelerStatusType = coiTravelerStatusType;
	}

	public Boolean getIsSponsoredTravel() {
		return isSponsoredTravel;
	}

	public void setIsSponsoredTravel(Boolean isSponsoredTravel) {
		this.isSponsoredTravel = isSponsoredTravel;
	}

	public String getTravelTitle() {
		return travelTitle;
	}

	public void setTravelTitle(String travelTitle) {
		this.travelTitle = travelTitle;
	}

	public String getPurposeOfTheTrip() {
		return purposeOfTheTrip;
	}

	public void setPurposeOfTheTrip(String purposeOfTheTrip) {
		this.purposeOfTheTrip = purposeOfTheTrip;
	}

	public BigDecimal getTravelAmount() {
		return travelAmount;
	}

	public void setTravelAmount(BigDecimal travelAmount) {
		this.travelAmount = travelAmount;
	}

	public Date getTravelStartDate() {
		return travelStartDate;
	}

	public void setTravelStartDate(Date travelStartDate) {
		this.travelStartDate = travelStartDate;
	}

	public Integer getNoOfDays() {
		return noOfDays;
	}

	public void setNoOfDays(Integer noOfDays) {
		this.noOfDays = noOfDays;
	}

	public String getDestinationCity() {
		return destinationCity;
	}

	public void setDestinationCity(String destinationCity) {
		this.destinationCity = destinationCity;
	}

	public String getDestinationCountry() {
		return destinationCountry;
	}

	public void setDestinationCountry(String destinationCountry) {
		this.destinationCountry = destinationCountry;
	}

	public String getRelationshipToYourResearch() {
		return relationshipToYourResearch;
	}

	public void setRelationshipToYourResearch(String relationshipToYourResearch) {
		this.relationshipToYourResearch = relationshipToYourResearch;
	}

	public String getAcknowledgeBy() {
		return acknowledgeBy;
	}

	public void setAcknowledgeBy(String acknowledgeBy) {
		this.acknowledgeBy = acknowledgeBy;
	}

	public String getAcknowledgeAt() {
		return acknowledgeAt;
	}

	public void setAcknowledgeAt(String acknowledgeAt) {
		this.acknowledgeAt = acknowledgeAt;
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

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

}