package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

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
import com.polus.fibicomp.negotiation.dto.AttachmentData;

@Entity
@Table(name = "NEGOTIATION_ACTIVITY")
public class NegotiationsActivity implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_NEGOTIATION_ACTIVITY")
	@SequenceGenerator(name = "SEQ_NEGOTIATION_ACTIVITY", sequenceName = "SEQ_NEGOTIATION_ACTIVITY", allocationSize = 1)
	@Column(name = "NEGOTIATION_ACTIVITY_ID")
	private Integer negotiationsActivityId;

	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ACTIVITY_FK3"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;

	@Column(name = "LOCATION_TYPE_CODE")
	private String locationTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ACTIVITY_FK4"), name = "LOCATION_TYPE_CODE", referencedColumnName = "LOCATION_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsLocationType negotiationsLocationType;

	@Column(name = "NEGOTIATION_LOCATION_ID")
	private Integer negotiationLocationId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ACTIVITY_FK2"), name = "NEGOTIATION_LOCATION_ID", referencedColumnName = "NEGOTIATION_LOCATION_ID", insertable = false, updatable = false)
	private NegotiationsLocation negotiationsLocation;

	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_ACTIVITY_FK1"), name = "ACTIVITY_TYPE_CODE", referencedColumnName = "ACTIVITY_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsActivityType negotiationsActivityType;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "CREATE_DATE")
	private Timestamp createDate;

	@Column(name = "FOLLOWUP_DATE")
	private Timestamp followupDate;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "RESTRICTED")
	private String restricted;

	@Transient
	private String acType;

	@Transient
	private Long noOfDays;

	@Transient
	private List<AttachmentData> attachmentDataList;

	@Transient
	private String updateUserFullName;

	public NegotiationsActivity() {
		attachmentDataList = new ArrayList<>();
	}

	public Integer getNegotiationsActivityId() {
		return negotiationsActivityId;
	}

	public void setNegotiationsActivityId(Integer negotiationsActivityId) {
		this.negotiationsActivityId = negotiationsActivityId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
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

	public Integer getNegotiationLocationId() {
		return negotiationLocationId;
	}

	public void setNegotiationLocationId(Integer negotiationLocationId) {
		this.negotiationLocationId = negotiationLocationId;
	}

	public NegotiationsLocation getNegotiationsLocation() {
		return negotiationsLocation;
	}

	public void setNegotiationsLocation(NegotiationsLocation negotiationsLocation) {
		this.negotiationsLocation = negotiationsLocation;
	}

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public NegotiationsActivityType getNegotiationsActivityType() {
		return negotiationsActivityType;
	}

	public void setNegotiationsActivityType(NegotiationsActivityType negotiationsActivityType) {
		this.negotiationsActivityType = negotiationsActivityType;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Timestamp getCreateDate() {
		return createDate;
	}

	public void setCreateDate(Timestamp createDate) {
		this.createDate = createDate;
	}

	public Timestamp getFollowupDate() {
		return followupDate;
	}

	public void setFollowupDate(Timestamp followupDate) {
		this.followupDate = followupDate;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public String getRestricted() {
		return restricted;
	}

	public void setRestricted(String restricted) {
		this.restricted = restricted;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Long getNoOfDays() {
		return noOfDays;
	}

	public void setNoOfDays(Long noOfDays) {
		this.noOfDays = noOfDays;
	}

	public List<AttachmentData> getAttachmentDataList() {
		return attachmentDataList;
	}

	public void setAttachmentDataList(List<AttachmentData> attachmentDataList) {
		this.attachmentDataList = attachmentDataList;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

}
