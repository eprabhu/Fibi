package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "COI_TRAVEL_DISCLOSURE_TRAVELER")
@EntityListeners(AuditingEntityListener.class)
public class CoiTravelDisclosureTraveler implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TRAVEL_TRAVELER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer travelTravelerId;
	
	@Column(name = "TRAVEL_DISCLOSURE_ID")
	private Integer travelDisclosureId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_TRAVELER_FK1"), name = "TRAVEL_DISCLOSURE_ID", referencedColumnName = "TRAVEL_DISCLOSURE_ID", insertable = false, updatable = false)
	private CoiTravelDisclosure coiTravelDisclosure;
	
	@Column(name = "TRAVELER_TYPE_CODE")
	private String travelerTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_TRAVELER_FK2"), name = "TRAVELER_TYPE_CODE", referencedColumnName = "TRAVELER_TYPE_CODE", insertable = false, updatable = false)
	private CoiTravelerType coiTravelerType;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getTravelTravelerId() {
		return travelTravelerId;
	}

	public void setTravelTravelerId(Integer travelTravelerId) {
		this.travelTravelerId = travelTravelerId;
	}

	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public String getTravelerTypeCode() {
		return travelerTypeCode;
	}

	public void setTravelerTypeCode(String travelerTypeCode) {
		this.travelerTypeCode = travelerTypeCode;
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

	public CoiTravelDisclosure getCoiTravelDisclosure() {
		return coiTravelDisclosure;
	}

	public void setCoiTravelDisclosure(CoiTravelDisclosure coiTravelDisclosure) {
		this.coiTravelDisclosure = coiTravelDisclosure;
	}

	public CoiTravelerType getCoiTravelerType() {
		return coiTravelerType;
	}

	public void setCoiTravelerType(CoiTravelerType coiTravelerType) {
		this.coiTravelerType = coiTravelerType;
	}

}
