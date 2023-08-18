package com.polus.fibicomp.coi.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;
import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CoiTravelDisclosureDto {

	private Integer travelDisclosureId;
	private Integer travelNumber;
	private String versionStatus;
	private Integer entityId;
	private Integer entityNumber;
	private String travelEntityName;
	private String entityEmail;
	private String entityAddress;
	private Boolean entityIsActive;
	private String travelTitle;
	private String purposeOfTheTrip;
	private BigDecimal travelAmount;
	private Date travelStartDate;
	private Date travelEndDate;
	private String destinationCity;
	private String destinationCountry;
	private String travelState;
	private String relationshipToYourResearch;
	private String acknowledgeBy;
	private Timestamp acknowledgeAt;
	private String travellerHomeUnit;
	private String description;
	private Date travelSubmissionDate;
	private String dispositionStatus;
	private String dispositionStatusCode;
	private String reviewStatus;
	private String reviewStatusCode;
	private String adminPersonId;
	private Integer adminGroupId;
	private String adminPersonName;
	private String adminGroupName;
	private String homeUnitNumber;
	private String homeUnitName;
	private Boolean isInterNationalTravel;
	private String personId;
	private String personFullName;
	private String entityTypeCode;
	private String entityType;
	private String countryCode;
	private String country;
	private String certifiedBy;
	private Timestamp certifiedAt;
	private String documentStatusCode;
	private String documentStatus;
	private String riskLevel;
	Map<String, String> travellerTypeCodeList;
	private Date expirationDate;
	private String disclosureStatusCode;
	private String disclosureStatus;
	private String createUser;
	private Timestamp createTimestamp;
	private String updateUser;
	private Timestamp updateTimestamp;
	private String comment;
	private String riskCategoryCode;

}
