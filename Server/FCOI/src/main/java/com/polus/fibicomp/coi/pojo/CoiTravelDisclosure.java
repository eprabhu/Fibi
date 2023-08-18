package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.Map;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "COI_TRAVEL_DISCLOSURE")
@EntityListeners(AuditingEntityListener.class)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CoiTravelDisclosure implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TRAVEL_DISCLOSURE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer travelDisclosureId;
	
	@Column(name = "TRAVEL_NUMBER")
	private Integer travelNumber;
	
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

	@Column(name = "RISK_CATEGORY_CODE")
	private String riskCategoryCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_TRAVEL_DISCLOSURE_FK5"), name = "RISK_CATEGORY_CODE", referencedColumnName = "RISK_CATEGORY_CODE", insertable = false, updatable = false)
	private CoiRiskCategory coiRiskCategory;

	@Column(name = "IS_SPONSORED_TRAVEL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSponsoredTravel;

	@Column(name = "IS_INTERNATIONAL_TRAVEL")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isInterNationalTravel;

	@Column(name = "TRAVEL_TITLE")
	private String travelTitle;

	@Column(name = "PURPOSE_OF_THE_TRIP")
	private String purposeOfTheTrip;

	@Column(name = "TRAVEL_AMOUNT", precision = 12, scale = 2)
	private BigDecimal travelAmount;

	@Column(name = "TRAVEL_START_DATE")
	private Date travelStartDate;

	@Column(name = "TRAVEL_END_DATE")
	private Date travelEndDate;

	@Column(name = "NO_OF_DAYS")
	private Integer noOfDays;

	@Column(name = "DESTINATION_CITY")
	private String destinationCity;

	@Column(name = "DESTINATION_COUNTRY")
	private String destinationCountry;

	@Column(name = "STATE")
	private String travelstate;

	@Column(name = "RELATIONSHIP_TO_YOUR_RESEARCH")
	private String relationshipToYourResearch;

	@Column(name = "ACKNOWLEDGE_BY")
	private String acknowledgeBy;

	@Column(name = "ACKNOWLEDGE_AT")
	private Timestamp acknowledgeAt;

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

	@Transient
	private List<String> coiTravellerTypeCodeList;

	@Column(name = "HOME_UNIT")
	private String travellerHomeUnit;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "SUBMISSION_DATE")
	private Date travelSubmissionDate;

	@Column(name = "TRAVEL_DISCLOSURE_STATUS_CODE")
	private String disclosureStatusCode;

	@Column(name = "DISPOSITION_STATUS_CODE")
	private String dispositionStatusCode;

	@Column(name = "REVIEW_STATUS_CODE")
	private String reviewStatusCode;

	@Column(name = "TRAVELER_TYPE_CODE")
	private String travellerTypeCode;

	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;

	@Column(name = "ADMIN_PERSON_ID")
	private String adminPersonId;

	@Column(name = "CERTIFIED_BY")
	private String certifiedBy;

	@Column(name = "CERTIFIED_AT")
	private Timestamp certifiedAt;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private String documentStatusCode;

	@Column(name = "EXPIRATION_DATE")
	private Date expirationDate;

	@Transient
	private Unit travellerUnitDetails;

	@Transient
	private CoiTravelDisclosureStatusType coiTravelDisclosureStatusTypeDetalis;

	@Transient
	private String adminGroupName;

	@Transient
	private String adminPersonName;

	@Transient
	private String personFullName;

	@Transient
	Map<String, String> travellerTypeCodeList;

	@Transient
	private CoiTravelDocumentStatusType coiDocumentStatusTypeDetalis;

	@Transient
	private CoiTravelReviewStatusType coiTravelReviewStatusTypeDetails;

	@Transient
	private CoiTravelDisclosureStatusType coiTravelDisclosureStatusList;

	@Transient
	private CoiEntity entityDetails;

	@Transient
	private String riskLevel;

}
