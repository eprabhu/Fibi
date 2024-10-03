package com.polus.fibicomp.coi.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DisclosureProjectDto {

	private Integer coiDisclProjectId;
	private Integer moduleCode;
	private String projectId;
	private String projectNumber;
	private String title;
	private String projectStatus;
	private Timestamp projectStartDate;
	private Timestamp projectEndDate;
	private String homeUnitNumber;
	private String homeUnitName;
	private String leadUnitNumber;
	private String leadUnitName;
	private String sponsorName;
	private String sponsorCode;
	private String primeSponsorName;
	private String primeSponsorCode;
	private String piName;
	private String keyPersonId;
	private String keyPersonName;
	private String keyPersonRole;
	private String reporterRole;
	private String conflictStatus;
	private String conflictStatusCode;
	private Integer entityCount;
	private Boolean relationShipExists;
	private Boolean conflictCompleted;
	private Map<Integer, Long> conflictCount;
	private String projectTypeCode;
	private String projectType;
	private String projectBadgeColour;
	private Boolean disclosureSubmitted;
	private Boolean questionnaireCompleted;
	private Boolean disclsoureNeeded;
	private String disclosureReviewStatus;
	private String submissionStatus;
	private Integer inCompletCount;
	private Integer completeCount;
	private Integer disclosureId;
	private Boolean trainingCompleted;
	private Boolean certification;
	private Boolean disclosureRequired;
	private Timestamp updateTimestamp;
	private String disclosureStatus;
	private Integer proposalCount;
	private Integer commentCount;
	private Integer keyPersonCount;
	private String projectIcon;
	private String certificationFlag;
	private String disclosureRequiredFlag;
	private List<CoiDisclEntProjDetailsDto> coiDisclEntProjDetails;
	private String accountNumber;
	private String documentNumber;
}
