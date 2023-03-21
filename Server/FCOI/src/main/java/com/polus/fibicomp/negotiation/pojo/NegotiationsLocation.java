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
import com.polus.fibicomp.person.pojo.Person;

@Entity
@Table(name = "NEGOTIATION_LOCATION")
public class NegotiationsLocation implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_NEGOTIATION_LOCATION")
	@SequenceGenerator(name = "SEQ_NEGOTIATION_LOCATION", sequenceName = "SEQ_NEGOTIATION_LOCATION", allocationSize = 1)
	@Column(name = "NEGOTIATION_LOCATION_ID")
	private Integer negotiationLocationId;

	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_LOCATION_FK2"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "LOCATION_TYPE_CODE")
	private String locationTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_LOCATION_FK1"), name = "LOCATION_TYPE_CODE", referencedColumnName = "LOCATION_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsLocationType negotiationsLocationType;

	@Column(name = "ASSIGNEE_PERSON_ID")
	private String assigneePersonId;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_LOCATION_FK3"), name = "ASSIGNEE_PERSON_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
	private Person person;

	@Column(name = "LOCATION_STATUS_CODE")
	private String locationStatusCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_LOCATION_FK4"), name = "LOCATION_STATUS_CODE", referencedColumnName = "LOCATION_STATUS_CODE", insertable = false, updatable = false)
	private NegotiationLocationStatus negotiationLocationStatus;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "DESCRIPTION")
	private String description;

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	@Transient
	private int numberOfDays;

	@Transient
	private String acType;

	@Transient
	private String createUserFullName;

	@Transient
	private String updateUserFullName;

	@Transient
	private Integer agreementRequestId;

	@Transient
	private Long totalComments;

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

	public Negotiations getNegotiations() {
		return negotiations;
	}

	public void setNegotiations(Negotiations negotiations) {
		this.negotiations = negotiations;
	}

	public String getLocationTypeCode() {
		return locationTypeCode;
	}

	public void setLocationTypeCode(String locationTypeCode) {
		this.locationTypeCode = locationTypeCode;
	}

	public NegotiationsLocationType getNegotiationsLocationType() {
		return negotiationsLocationType;
	}

	public void setNegotiationsLocationType(NegotiationsLocationType negotiationsLocationType) {
		this.negotiationsLocationType = negotiationsLocationType;
	}

	public int getNumberOfDays() {
		return numberOfDays;
	}

	public void setNumberOfDays(int numberOfDays) {
		this.numberOfDays = numberOfDays;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getNegotiationLocationId() {
		return negotiationLocationId;
	}

	public void setNegotiationLocationId(Integer negotiationLocationId) {
		this.negotiationLocationId = negotiationLocationId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Person getPerson() {
		return person;
	}

	public void setPerson(Person person) {
		this.person = person;
	}

	public String getLocationStatusCode() {
		return locationStatusCode;
	}

	public void setLocationStatusCode(String locationStatusCode) {
		this.locationStatusCode = locationStatusCode;
	}

	public NegotiationLocationStatus getNegotiationLocationStatus() {
		return negotiationLocationStatus;
	}

	public void setNegotiationLocationStatus(NegotiationLocationStatus negotiationLocationStatus) {
		this.negotiationLocationStatus = negotiationLocationStatus;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getAssigneePersonId() {
		return assigneePersonId;
	}

	public void setAssigneePersonId(String assigneePersonId) {
		this.assigneePersonId = assigneePersonId;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public Long getTotalComments() {
		return totalComments;
	}

	public void setTotalComments(Long totalComments) {
		this.totalComments = totalComments;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

}
