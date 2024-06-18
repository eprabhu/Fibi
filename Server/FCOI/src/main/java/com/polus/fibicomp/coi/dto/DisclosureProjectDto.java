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

	private Integer moduleCode;
	private String projectId;
	private String projectNumber;
	private String title;
	private String projectStatus;
	private Timestamp projectStartDate;
	private Timestamp projectEndDate;
	private String homeUnitNumber;
	private String homeUnitName;
	private String sponsorName;
	private String primeSponsorName;
	private String piName;
	private String keyPersonId;
	private String keyPersonName;
	private String reporterRole;
	private String conflictStatus;
	private String conflictStatusCode;
	private Integer entityCount;
	private Boolean relationShipExists;
	private Boolean sfiCompleted;
	private List<Map<Object, Object>> disclosureStatusCount;
	private String projectTypeCode;
	private String projectType;
	private String projectBadgeColour;
}
