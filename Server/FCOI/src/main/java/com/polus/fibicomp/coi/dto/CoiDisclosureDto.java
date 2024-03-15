package com.polus.fibicomp.coi.dto;


import java.sql.Timestamp;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CoiDisclosureDto {

	private Integer disclosureId;
	private String personId;
	private String homeUnit;
	private String homeUnitName;
	private Integer disclosureNumber;
	private Integer versionNumber;
	private String versionStatus;
	private String conflictStatusCode;
	private String conflictStatus;
	private String dispositionStatusCode;
	private String dispositionStatus;
	private String reviewStatusCode;
	private String reviewStatus;
	private Date certifiedAt;
	private Date expirationDate;
	private Timestamp updateTimestamp;
	private Timestamp createTimestamp;
	private String updateUserFullName;
	private String createUserFullName;
	private Integer adminGroupId;
	private String adminPersonId;
	private String disclosurePersonFullName;
	private String adminGroupName;
	private String adminPersonName;
	private String riskCategoryCode;
	private String revisionComment;
	private String actionType;

}
